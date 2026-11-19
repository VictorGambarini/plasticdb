# Jobs: submit an analysis, wait for it, read or download its results, share
# it. Mirrors plasticdb-py's resources/jobs.py.

# The job types POST /jobs runs, and the form fields each one takes.
SEARCH_PARAMS <- c("blast_type", "organism_type", "evalue", "pident",
    "include_variants")
VIEW_PARAMS <- c("min_qcov", "min_scov", "min_bitscore")
JOB_PARAMS <- list(
    annotate_gene = c(SEARCH_PARAMS, VIEW_PARAMS),
    annotate_genome = c(SEARCH_PARAMS, VIEW_PARAMS),
    compare_genomes = c(SEARCH_PARAMS, VIEW_PARAMS),
    annotate_list = c("genus_column", "species_column", "field_separator"),
    pathway_analysis = c("target_plastic", "blast_type", "evalue", "pident",
        "min_step_identity", "max_step_evalue", "min_reference_coverage",
        "min_query_coverage"),
    hmm_screen = c("target_polymers", "threshold_mode", "evalue")
)
# POST /tree/build's filters (`class` may be spelled `class_`).
TREE_FILTERS <- c("microorganism_ids", "q", "name", "plastic", "plastic_id",
    "include_subtypes", "tax_id", "temp_class", "environment", "location",
    "year", "min_year", "max_year", "lineage", "domain", "kingdom", "phylum",
    "class", "order", "family", "genus", "evidence_method", "substrate_form",
    "include_negative", "polarity", "evidence_scope", "sole_carbon_source",
    "aerobicity", "updated_since")
JOB_TYPES <- c(names(JOB_PARAMS), "build_tree")
FINAL_STATUSES <- c("done", "error")

# Polls and reads leave the hits out; job_results() pages them.
LEAN <- list(include = "none")

#' Run PlasticDB's analyses (jobs)
#'
#' Jobs run PlasticDB's analyses on the server: DIAMOND searches of genes,
#' genomes and genome sets against PlasticDB's proteins, the matching of a
#' table of taxa, pathway mapping, HMM screens and taxonomy trees. They need
#' an API key (`plasticdb_client(api_key = )` or `PLASTICDB_API_KEY`;
#' without one the functions signal `plasticdb_api_key_required` before
#' anything is sent). Reading a shared job ([job_shared()]) needs none.
#'
#' `submit_job()` submits a job by its server name and returns a
#' `plasticdb_job` at once; the typed helpers (`annotate_gene()`,
#' `annotate_genome()`, `compare_genomes()`, `annotate_list()`,
#' `pathway_analysis()`, `hmm_screen()`, `build_tree()`) name and document
#' each job type's parameters. Then [wait_job()] polls until it finishes and
#' [job_results()] returns its results. `search_sequence()` is synchronous:
#' it searches one sequence now and returns the hits.
#'
#' **Inputs.** A FASTA input is a file path (plain or gzip), FASTA text (a
#' string starting with `>`, or a character vector of lines), a single line
#' of residues, raw bytes, or a [Biostrings::AAStringSet] /
#' [Biostrings::DNAStringSet] (use `blast_type = "blastx"` for nucleotide
#' input). A taxa table is a path, text or a data frame (sent tab-separated,
#' with its column names as the header row). Text and sets are written to
#' temporary files; every upload is named after its contents (`.gz` exactly
#' when it is gzip), as the server expects.
#'
#' **Releases.** A pinned client (or `release =`) runs the job against that
#' release's archived reference databases; a job type or option the release
#' cannot answer signals `plasticdb_release_unsupported_filter`.
#'
#' **Retries and idempotency.** Each submission is sent with an
#' `Idempotency-Key` header: `idempotency_key`, or a new UUID for each call,
#' and the same key on every retry of that call. The server keeps a key for
#' 24 hours per user and answers a repeat with the job the first request
#' created (`job_replayed()` is then `TRUE`), so with an API key a submission
#' is retried like a read: on a timeout, a dropped connection or a 5xx. Pass
#' your own key when a script may be re-run after a crash: the re-run gets
#' the first job back instead of a duplicate. When the retries run out, the
#' error names the key to resubmit with (field `idempotency_key`). The same
#' key with a different request signals
#' `plasticdb_idempotency_key_reused`.
#'
#' **Limits.** Submissions are rate-limited (30 an hour); a 429 is waited
#' out when `Retry-After` is within `max_retry_wait`. A file over the size
#' limit, or more than 20 genomes, is `plasticdb_payload_too_large`.
#'
#' @param type A job type: `"annotate_gene"`, `"annotate_genome"`,
#'   `"compare_genomes"`, `"annotate_list"`, `"pathway_analysis"`,
#'   `"hmm_screen"` or `"build_tree"` (dashes may stand for underscores).
#' @param input What the job reads: a FASTA input, a list of genomes
#'   (`compare_genomes`) or a taxa table (`annotate_list`); see Details.
#'   `build_tree` takes filters in `...` instead.
#' @param ... For `submit_job()`, the job type's parameters (as named for its
#'   helper; `labels` for `compare_genomes`); for `build_tree()`, filters.
#' @param fasta A FASTA input (see Details).
#' @param genomes Two or more genome inputs (up to 20), as a list or a
#'   character vector of paths. Names label the genomes in the results.
#' @param labels The genomes' labels, instead of their names. Otherwise a
#'   genome is labelled by its file name, else `genome_<n>`.
#' @param table A taxa table: a path, text or data frame.
#' @param sequence One sequence: residues, one-record FASTA text, or a
#'   Biostrings sequence (or a set of one).
#' @param blast_type `"blastp"` (protein input, the default) or `"blastx"`
#'   (nucleotide input).
#' @param organism_type Signal-peptide prediction for the hits:
#'   `"no_signalp"` (default), `"gram-"`, `"gram+"`, `"arch"` or `"euk"`.
#' @param evalue E-value threshold (default 1e-5). For `hmm_screen()` it
#'   applies only with `threshold_mode = "evalue"`.
#' @param pident Minimum percent identity (default 30).
#' @param include_variants Also search engineered variants.
#' @param min_qcov,min_scov,min_bitscore The default view of the results
#'   file and page: hide hits covering less than this % of the query /
#'   reference (`min_scov` defaults to 70), or under this bitscore. Every hit
#'   is stored; [job_results()] returns them all.
#' @param genus_column,species_column 1-based columns holding the genus and
#'   species. They default to 7 and 8 (the server's layout), so pass them for
#'   any other table.
#' @param field_separator `"tab"` (default), `"comma"` or `"space"`.
#' @param target_plastic The plastic whose degradation pathway is mapped
#'   (default `"PET"`).
#' @param min_step_identity,max_step_evalue When a hit calls a pathway step
#'   present: its minimum % identity (default 40) and maximum e-value
#'   (default 1e-10).
#' @param min_reference_coverage,min_query_coverage And the minimum % of the
#'   reference enzyme (default 70) and of your protein (default 50) the
#'   alignment must cover.
#' @param target_polymers Polymers to limit the HMM polymer summary to, e.g.
#'   `c("PET", "PLA")`.
#' @param threshold_mode `"gathering"` (default: each profile's calibrated
#'   cut-off) or `"evalue"` (`evalue` only).
#' @param release A release version to run against, instead of the client's.
#' @param idempotency_key Identifies this submission (any 1-255 printable
#'   ASCII characters; default a new UUID): resending it returns the same job.
#' @param client A [plasticdb_client()] with an API key.
#'
#' @return A `plasticdb_job`: the job as the server describes it (a list:
#'   `id`, `job_type`, `status`, `release`, ...). `search_sequence()`
#'   returns a list with the `hits` (a tibble) and what they were searched
#'   against (`release`, `release_source`, `reference_sha256`,
#'   `diamond_version`).
#' @seealso [wait_job()], [job_results()], [job_download()], [job_share()],
#'   [jobs_list()], [job_provenance()]
#' @name jobs
#' @examples
#' ## Arguments are checked before anything is sent:
#' res <- tryCatch(submit_job("no_such_job", ">q\nMKVLAAGIVALLLAAGCSS"),
#'     plasticdb_argument_error = function(e) conditionMessage(e))
#' res
#' \donttest{
#' ## Jobs need an API key (https://v2.plasticdb.org/profile/api-keys).
#' if (nzchar(Sys.getenv("PLASTICDB_API_KEY"))) {
#'     client <- plasticdb_client()
#'     genome <- tempfile(fileext = ".faa")
#'     seq <- paste0("MNFPRASRLMQAAVLGGLMAVSAAATAQ",
#'         "TNPYARGPNPTAASLEASAGPF")
#'     writeLines(c(">p1", seq), genome)
#'     job <- annotate_genome(genome, client = client)
#'     job <- wait_job(job, client = client)
#'     hits <- job_results(job, client = client)
#'     cmp <- compare_genomes(list(A = genome, B = genome), client = client)
#'     tree <- job_results(wait_job(build_tree(plastic = "PET", genus =
#'         "Pseudomonas", client = client), client = client), client = client)
#'     search_sequence("MNFPRASRLMQAAVLGGLMAVSAAATAQTNPYARGPNPTAASLEASAGPF",
#'         client = client)$hits
#' }
#' }
NULL

normalise_job_type <- function(type) {
    if (!is.character(type) || length(type) != 1L || is.na(type)) {
        arg_error("`type` must be a job type name.")
    }
    name <- gsub("-", "_", tolower(trimws(type)), fixed = TRUE)
    if (name %in% c("taxonomy_tree", "tree")) "build_tree" else name
}

require_api_key <- function(client, what = "Jobs") {
    if (is.null(client$api_key)) {
        pdb_abort("api_key_required", c(
            sprintf("%s need an API key.", what),
            i = paste("Pass plasticdb_client(api_key = ...) or set",
                "PLASTICDB_API_KEY."),
            i = sprintf("Create a key at %s/profile/api-keys.",
                client$root_url)), status = 401L)
    }
    invisible(client)
}

check_job_params <- function(params, allowed, where) {
    given <- names(params)
    if (length(params) && (is.null(given) || any(!nzchar(given)))) {
        arg_error(sprintf("%s: every parameter must be named.", where))
    }
    unknown <- setdiff(given, allowed)
    if (length(unknown)) {
        arg_error(c(sprintf("%s got unknown parameter(s): %s.", where,
            paste(unknown, collapse = ", ")),
            i = paste("Valid:", paste(sort(allowed), collapse = ", "))))
    }
    Filter(Negate(is.null), params)
}

form_values <- function(value) {
    if (is.logical(value)) {
        return(as.list(ifelse(value, "true", "false")))
    }
    as.list(query_value(value))
}

#' @rdname jobs
#' @export
submit_job <- function(type, input = NULL, ..., release = NULL,
                        idempotency_key = NULL, client = plasticdb_client()) {
    client <- check_client(client)
    kind <- normalise_job_type(type)
    params <- list(...)
    if (kind == "build_tree") {
        if (!is.null(input)) {
            arg_error("build_tree takes filters, not an input file.")
        }
        return(do.call(build_tree, c(params, list(release = release,
            idempotency_key = idempotency_key, client = client))))
    }
    if (kind == "search_sequence") {
        arg_error("search_sequence is synchronous: use search_sequence().")
    }
    if (!kind %in% names(JOB_PARAMS)) {
        arg_error(sprintf("Unknown job type \"%s\"; valid: %s.", type,
            paste(JOB_TYPES, collapse = ", ")))
    }
    labels <- NULL
    if (kind == "compare_genomes") {
        labels <- params$labels
        params$labels <- NULL
    }
    params <- check_job_params(params, JOB_PARAMS[[kind]],
        sprintf("submit_job(\"%s\")", kind))
    require_api_key(client)
    if (is.null(input)) {
        arg_error(sprintf("%s needs an input.", kind))
    }
    dir <- tempfile("plasticdb-upload-")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    uploads <- if (kind == "compare_genomes") {
        genome_uploads(input, labels, dir)
    } else if (kind == "annotate_list") {
        list(to_upload(input, "tsv", dir, kind = "taxa table"))
    } else {
        list(to_upload(input, "fasta", dir))
    }
    post_job(client, kind, uploads, params, release, idempotency_key)
}

post_job <- function(client, kind, uploads, params, release,
                        idempotency_key) {
    field <- if (kind == "compare_genomes") "files" else "file"
    type <- if (kind == "annotate_list") "text/tab-separated-values" else
        "text/plain"
    fields <- list(job_type = kind)
    for (name in names(params)) {
        for (value in form_values(params[[name]])) {
            fields[[length(fields) + 1L]] <- value
            names(fields)[[length(fields)]] <- name
        }
    }
    for (upload in uploads) {
        gz <- is_gzip_file(upload$path)
        fields[[length(fields) + 1L]] <- curl::form_file(upload$path,
            type = if (gz) "application/gzip" else type, name = upload$name)
        names(fields)[[length(fields)]] <- field
    }
    key <- idempotency_key %||% new_idempotency_key()
    created(client, key, function() pdb_request(client, ENDPOINTS$jobs_submit,
        params = list(release = release), body = list(multipart = fields),
        idempotency_key = key))
}

# Send a create; on a lost answer, say which key recovers the job.
created <- function(client, key, send) {
    result <- tryCatch(send(),
        plasticdb_connection_error = function(e) lost_submission(e, key),
        plasticdb_server_error = function(e) lost_submission(e, key))
    job <- new_job(result$data, result)
    if (isTRUE(result$replayed) && !isTRUE(getOption("plasticdb.quiet"))) {
        rlang::inform(sprintf(paste("PlasticDB: idempotency key %s returned",
            "the existing job %s."), key, job$id),
            class = "plasticdb_replayed")
    }
    job
}

lost_submission <- function(e, key) {
    e$message <- paste0(conditionMessage(e), " The submission may have ",
        "reached the server: submit it again with idempotency_key = \"",
        key, "\" (within 24 hours) to get that job back rather than a ",
        "duplicate.")
    e$idempotency_key <- key
    rlang::cnd_signal(e)
}

new_job <- function(data, result, token = NULL) {
    if (!is_object(data) || is.null(data$id)) {
        pdb_abort("unexpected_response", "The server did not answer a job.",
            url = result$url)
    }
    class(data) <- c("plasticdb_job", "list")
    attr(data, "plasticdb") <- list(release = result$release,
        release_source = result$release_source, url = result$url,
        replayed = isTRUE(result$replayed), token = token,
        retrieved_at = result$retrieved_at)
    data
}

#' @export
print.plasticdb_job <- function(x, ...) {
    meta <- attr(x, "plasticdb")
    release <- x$release %||% meta$release
    cat(sprintf("<plasticdb_job %s %s %s%s%s>\n", x$id, x$job_type, x$status,
        if (!is.null(release)) paste0(" release=", release) else "",
        if (!is.null(meta$token)) " (shared)" else ""))
    if (!is.null(x$error_message)) cat("  error: ", x$error_message, "\n",
        sep = "")
    if (!is.null(x$result_summary)) cat("  ", x$result_summary, "\n",
        sep = "")
    invisible(x)
}

#' @rdname jobs
#' @export
annotate_gene <- function(fasta, blast_type = NULL, organism_type = NULL,
                            evalue = NULL, pident = NULL,
                            include_variants = NULL, min_qcov = NULL,
                            min_scov = NULL, min_bitscore = NULL,
                            release = NULL, idempotency_key = NULL,
                            client = plasticdb_client()) {
    submit_job("annotate_gene", fasta, blast_type = blast_type,
        organism_type = organism_type, evalue = evalue, pident = pident,
        include_variants = include_variants, min_qcov = min_qcov,
        min_scov = min_scov, min_bitscore = min_bitscore, release = release,
        idempotency_key = idempotency_key, client = client)
}

#' @rdname jobs
#' @export
annotate_genome <- function(fasta, blast_type = NULL, organism_type = NULL,
                            evalue = NULL, pident = NULL,
                            include_variants = NULL, min_qcov = NULL,
                            min_scov = NULL, min_bitscore = NULL,
                            release = NULL, idempotency_key = NULL,
                            client = plasticdb_client()) {
    submit_job("annotate_genome", fasta, blast_type = blast_type,
        organism_type = organism_type, evalue = evalue, pident = pident,
        include_variants = include_variants, min_qcov = min_qcov,
        min_scov = min_scov, min_bitscore = min_bitscore, release = release,
        idempotency_key = idempotency_key, client = client)
}

#' @rdname jobs
#' @export
compare_genomes <- function(genomes, labels = NULL, blast_type = NULL,
                            organism_type = NULL, evalue = NULL,
                            pident = NULL, include_variants = NULL,
                            min_qcov = NULL, min_scov = NULL,
                            min_bitscore = NULL, release = NULL,
                            idempotency_key = NULL,
                            client = plasticdb_client()) {
    submit_job("compare_genomes", genomes, labels = labels,
        blast_type = blast_type, organism_type = organism_type,
        evalue = evalue, pident = pident, include_variants = include_variants,
        min_qcov = min_qcov, min_scov = min_scov, min_bitscore = min_bitscore,
        release = release, idempotency_key = idempotency_key, client = client)
}

#' @rdname jobs
#' @export
annotate_list <- function(table, genus_column = NULL, species_column = NULL,
                            field_separator = NULL, release = NULL,
                            idempotency_key = NULL,
                            client = plasticdb_client()) {
    if (!is.null(field_separator)) {
        field_separator <- match.arg(field_separator,
            c("tab", "comma", "space"))
    }
    submit_job("annotate_list", table, genus_column = genus_column,
        species_column = species_column, field_separator = field_separator,
        release = release, idempotency_key = idempotency_key, client = client)
}

#' @rdname jobs
#' @export
pathway_analysis <- function(fasta, target_plastic = NULL, blast_type = NULL,
                                evalue = NULL, pident = NULL,
                                min_step_identity = NULL,
                                max_step_evalue = NULL,
                                min_reference_coverage = NULL,
                                min_query_coverage = NULL, release = NULL,
                                idempotency_key = NULL,
                                client = plasticdb_client()) {
    submit_job("pathway_analysis", fasta, target_plastic = target_plastic,
        blast_type = blast_type, evalue = evalue, pident = pident,
        min_step_identity = min_step_identity,
        max_step_evalue = max_step_evalue,
        min_reference_coverage = min_reference_coverage,
        min_query_coverage = min_query_coverage, release = release,
        idempotency_key = idempotency_key, client = client)
}

#' @rdname jobs
#' @export
hmm_screen <- function(fasta, target_polymers = NULL, threshold_mode = NULL,
                        evalue = NULL, release = NULL, idempotency_key = NULL,
                        client = plasticdb_client()) {
    if (!is.null(threshold_mode)) {
        threshold_mode <- match.arg(threshold_mode, c("gathering", "evalue"))
    }
    submit_job("hmm_screen", fasta,
        target_polymers = if (!is.null(target_polymers))
            as.character(target_polymers),
        threshold_mode = threshold_mode, evalue = evalue, release = release,
        idempotency_key = idempotency_key, client = client)
}

#' @rdname jobs
#' @export
build_tree <- function(..., release = NULL, idempotency_key = NULL,
                        client = plasticdb_client()) {
    client <- check_client(client)
    filters <- check_filters(list(...), TREE_FILTERS, "build_tree")
    require_api_key(client)
    body <- list()
    for (name in names(filters)) {
        value <- filters[[name]]
        if (inherits(value, c("POSIXt", "Date"))) {
            value <- query_value(value)
        }
        body[[name]] <- if (name == "microorganism_ids")
            as.list(as.integer(value)) else value
    }
    if (!length(body)) body <- structure(list(), names = character())
    key <- idempotency_key %||% new_idempotency_key()
    created(client, key, function() pdb_request(client, ENDPOINTS$tree_build,
        params = list(release = release), body = list(json = body),
        idempotency_key = key))
}

#' @rdname jobs
#' @export
search_sequence <- function(sequence, blast_type = NULL, evalue = NULL,
                            pident = NULL, include_variants = NULL,
                            release = NULL, client = plasticdb_client()) {
    client <- check_client(client)
    require_api_key(client, "Sequence searches")
    params <- list(sequence = sequence_text(sequence), release = release,
        blast_type = blast_type, evalue = evalue, pident = pident,
        include_variants = include_variants)
    result <- pdb_request(client, ENDPOINTS$search_sequence, params)
    data <- result$data %||% list()
    out <- list(hits = records_to_tibble(data$hits %||% list()),
        release = data$release, release_source = data$release_source,
        reference_sha256 = data$reference_sha256,
        diamond_version = data$diamond_version)
    as_record(out, client, result)
}

# ---- reading ---------------------------------------------------------------

job_token <- function(job) {
    if (inherits(job, "plasticdb_job")) attr(job, "plasticdb")$token
}

job_id_of <- function(job) {
    if (inherits(job, "plasticdb_job")) {
        return(job$id)
    }
    if (is.character(job) && length(job) == 1L && !is.na(job) &&
        nzchar(trimws(job))) {
        return(trimws(job))
    }
    arg_error("`job` must be a plasticdb_job or a job id.")
}

job_ref <- function(job) {
    token <- job_token(job)
    if (!is.null(token)) list(token = token) else list(job_id = job_id_of(job))
}

job_endpoint <- function(job, own, shared) {
    if (is.null(job_token(job))) ENDPOINTS[[own]] else ENDPOINTS[[shared]]
}

as_job <- function(job, client) {
    if (inherits(job, "plasticdb_job")) job else job_get(job, client = client)
}

#' Follow, read and share a job
#'
#' * `job_get()` fetches a job (yours, or one submitted without an account)
#'   by id, or refreshes a `plasticdb_job`: its status, summary and
#'   provenance, not its hits. `job_status()` returns just the status:
#'   `"waiting"`, `"running"`, `"done"` or `"error"`.
#' * `wait_job()` polls until the job finishes and returns it: every
#'   `poll_interval` seconds, backing off by half each time up to
#'   `max_poll_interval`; a 429 waits for the server's `Retry-After`. Past
#'   `timeout` seconds it signals `plasticdb_job_timeout` (the job keeps
#'   running: call `wait_job()` again, or `job_get()` later). A failed job
#'   signals `plasticdb_job_failed` (field `job`, the server's reason as the
#'   message) unless `error = FALSE`.
#' * `job_results()` returns the results by job type: for `annotate_gene`,
#'   `annotate_genome`, `compare_genomes` (`source_label` names the genome)
#'   and `hmm_screen`, every stored hit as a tibble, paged from the server
#'   500 at a time (the results file and page also hide hits under 70 %
#'   reference coverage by default; this does not). An `hmm_screen` hit is
#'   a profile match, not an alignment: `pident`, `length`, `qcov` and
#'   `scov` are always `NA` (never 0), jobs run before server 2.90.0
#'   included, so identity and coverage filters do not apply to them (test
#'   `is.na()` or skip them). `subject_seq_id` is the profile name and
#'   `profile_accession` its accession; `evalue` and `bitscore` are the
#'   full-sequence values, `domain_score` (`double`), `domain_c_evalue` and
#'   `domain_i_evalue` those of the best domain, `hmm_from`/`hmm_to` (its span
#'   on the profile) and `env_from`/`env_to` (its envelope on your sequence;
#'   1-based, inclusive) are `integer`. All of these are `NA` on DIAMOND hits
#'   and on HMM jobs run before 2.90.0. The HMM TSV from `job_download()`
#'   has 8 more columns after `calibration_status`: read it by header name.
#'   For `pathway_analysis`
#'   one row per pathway step (`present`, the best hit); for
#'   `annotate_list` your table's rows with PlasticDB's columns added; for
#'   `build_tree` a `plasticdb_tree`: `newick` (the cladogram as Newick
#'   text), `matrix` (organism x plastic, `TRUE` where it acts on the
#'   plastic) and `phylo` (an `ape::phylo` when \pkg{ape} is installed and
#'   `phylo = TRUE`, else `NULL`).
#' * `job_comparison()` is a `compare_genomes` job's genome x plastic matrix
#'   for a view of the hits (with no thresholds, the job's default view).
#' * `job_download()` saves the server's results file: a TSV with `#`
#'   provenance lines, or a ZIP for `compare_genomes` (per-genome hit tables,
#'   the matrix and `provenance.json`). Hit tables apply the job's default
#'   view unless you pass thresholds (`0` turns one off) or
#'   `all_hits = TRUE`. Result files are deleted under the server's
#'   retention policy; then it signals `plasticdb_results_expired` (the
#'   record and hits stay readable).
#' * `job_share()` creates a read-only link to the job's results. Its token
#'   is in the answer once only: keep `$url`. It is sent with an
#'   `Idempotency-Key`, so a retry returns the same link. `job_shares()`
#'   lists the job's links (never their tokens), `job_unshare()` revokes one
#'   at once. These three are for the job's owner (an API key); a job opened
#'   from a link is read-only (`plasticdb_permission_denied`).
#' * `job_shared()` opens a job someone shared, from its link or token (no
#'   API key needed); the other functions then read it through the link.
#' * `jobs_list()` lists your jobs, newest first.
#' * `job_provenance()` is what the job ran against, to report with its
#'   results: the dataset `release` and `release_source` (`"snapshot"` for
#'   a past release's archived databases, `"live"` for the live one; the
#'   job's own, whichever client reads it), `reference_sha256` (the DIAMOND
#'   database or HMM library searched), `tool_versions`, `app_version` and
#'   the server's full provenance `record`. `job_replayed()` is `TRUE` when
#'   the submission's idempotency key returned an existing job.
#'
#' @param job A `plasticdb_job`, or a job id.
#' @param timeout Seconds to wait before signalling `plasticdb_job_timeout`;
#'   `NULL` waits as long as it takes.
#' @param poll_interval,max_poll_interval The first and the longest wait
#'   between polls, in seconds.
#' @param error Signal `plasticdb_job_failed` for a failed job; with `FALSE`
#'   it is returned.
#' @param limit The most hits (or jobs) to return.
#' @param phylo For a tree, also parse it with `ape::read.tree()`.
#' @param scov,qcov,bitscore Hit thresholds for the view (`0` turns one off;
#'   `NULL` keeps the job's default).
#' @param all_hits Count or write every hit, with no view.
#' @param path A file, or a directory to save the server's file name in.
#' @param expires_in_days Days the link lasts (1-365); `NULL` for as long as
#'   the job.
#' @param share A share (a row of `job_shares()`, or `job_share()`'s answer)
#'   or its numeric id.
#' @param token_or_url A share link, or its token.
#' @param idempotency_key As for [submit_job()].
#' @param client A [plasticdb_client()].
#'
#' @return `job_get()`, `wait_job()` and `job_shared()` a `plasticdb_job`;
#'   `job_status()` a string; `job_results()` a tibble with provenance (or a
#'   `plasticdb_tree`); `job_comparison()` a list with the `genomes` tibble;
#'   `job_download()` the path written; `job_share()` a list (`token`,
#'   `url`, ...); `job_shares()` and `jobs_list()` tibbles;
#'   `job_unshare()` `TRUE`, invisibly; `job_provenance()` a list;
#'   `job_replayed()` `TRUE` or `FALSE`.
#' @seealso [jobs] to submit one.
#' @name job_results
#' @examples
#' ## A job opened from a share link needs no API key; an unknown link
#' ## is a classed error:
#' res <- tryCatch(job_shared("https://v2.plasticdb.org/jobs/shared/no-such"),
#'     plasticdb_not_found = function(e) conditionMessage(e))
#' res
#' \donttest{
#' ## Your own jobs need an API key; this uses your most recent finished job.
#' if (nzchar(Sys.getenv("PLASTICDB_API_KEY"))) {
#'     client <- plasticdb_client()
#'     recent <- jobs_list(limit = 5, client = client)
#'     recent <- recent[recent$status == "done", ]
#'     if (nrow(recent)) {
#'         job <- wait_job(recent$id[[1]], timeout = 3600, client = client)
#'         hits <- job_results(job, client = client)
#'         job_provenance(job)$reference_sha256
#'         job_download(job, tempdir(), client = client)
#'         link <- job_share(job, expires_in_days = 30, client = client)
#'         shared <- job_shared(link$url)
#'         job_results(shared)
#'     }
#' }
#' }
NULL

#' @rdname job_results
#' @export
job_get <- function(job, client = plasticdb_client()) {
    token <- job_token(job)
    endpoint <- job_endpoint(job, "jobs_get", "shared_get")
    result <- pdb_request(client, endpoint, LEAN, job_ref(job))
    new_job(result$data, result, token = token)
}

#' @rdname job_results
#' @export
job_status <- function(job, client = plasticdb_client()) {
    job_get(job, client = client)$status
}

# Wrapped so tests can move the clock.
pdb_now <- function() {
    Sys.time()
}

#' @rdname job_results
#' @export
wait_job <- function(job, timeout = NULL, poll_interval = 2,
                        max_poll_interval = 30, error = TRUE,
                        client = plasticdb_client()) {
    if (!is.numeric(poll_interval) || !is.numeric(max_poll_interval) ||
        poll_interval <= 0 || max_poll_interval <= 0) {
        arg_error("Poll intervals must be positive.")
    }
    deadline <- if (!is.null(timeout)) pdb_now() + timeout
    sleep_until <- function(seconds, current) {
        if (!is.null(deadline)) {
            left <- as.numeric(difftime(deadline, pdb_now(), units = "secs"))
            if (left <= 0) {
                pdb_abort("job_timeout", c(sprintf(
                    "Job %s is still %s after the timeout.", current$id,
                    current$status), i = paste("It keeps running on the",
                    "server: call wait_job() again, or job_get() later.")),
                    job = current)
            }
            seconds <- min(seconds, left)
        }
        pdb_sleep(seconds)
    }
    current <- if (inherits(job, "plasticdb_job")) job else NULL
    interval <- poll_interval
    first <- TRUE
    repeat {
        if (!(first && !is.null(current) &&
            current$status %in% FINAL_STATUSES)) {
            fresh <- tryCatch(job_get(current %||% job, client = client),
                plasticdb_rate_limit = function(e) e)
            if (inherits(fresh, "plasticdb_rate_limit")) {
                sleep_until(fresh$retry_after %||% max_poll_interval,
                    current %||% list(id = job_id_of(job), status = "waiting"))
                next
            }
            current <- fresh
        }
        first <- FALSE
        if (current$status %in% FINAL_STATUSES) break
        sleep_until(interval, current)
        interval <- min(interval * 1.5, max_poll_interval)
    }
    if (isTRUE(error) && identical(current$status, "error")) {
        job_failed(current)
    }
    current
}

job_failed <- function(job) {
    pdb_abort("job_failed", sprintf("Job %s (%s) failed: %s", job$id,
        job$job_type, job$error_message %||% "no reason given"), job = job)
}

require_done <- function(job, client) {
    job <- as_job(job, client)
    if (identical(job$status, "error")) job_failed(job)
    if (!identical(job$status, "done")) job <- job_get(job, client = client)
    if (identical(job$status, "error")) job_failed(job)
    if (!identical(job$status, "done")) {
        pdb_abort("job_error", sprintf(
            "Job %s is still %s: call wait_job() first.", job$id, job$status),
            job = job)
    }
    job
}

#' @rdname job_results
#' @export
job_results <- function(job, limit = Inf, phylo = TRUE,
                        client = plasticdb_client()) {
    job <- require_done(job, client)
    switch(job$job_type,
        pathway_analysis = job_steps(job, client),
        annotate_list = job_taxa(job, client),
        build_tree = job_tree(job, phylo, client),
        job_hits(job, limit, client))
}

job_hits <- function(job, limit, client) {
    endpoint <- job_endpoint(job, "jobs_results", "shared_results")
    list_tibble(client, endpoint, path_params = job_ref(job), limit = limit,
        kind = "job_hit")
}

job_result_provenance <- function(x, job, client) {
    meta <- attr(job, "plasticdb")
    result <- list(release = job$release %||% meta$release,
        release_source = job$release_source %||% meta$release_source,
        url = meta$url, from_cache = FALSE, unavailable_fields = character(),
        retrieved_at = meta$retrieved_at)
    with_provenance(x, client, result)
}

job_steps <- function(job, client) {
    table <- job$result_metadata$step_table %||% list()
    job_result_provenance(records_to_tibble(table), job, client)
}

job_taxa <- function(job, client) {
    endpoint <- job_endpoint(job, "jobs_download", "shared_download")
    result <- pdb_request(client, endpoint, path_params = job_ref(job))
    text <- if (is.raw(result$data)) rawToChar(result$data) else
        paste(as.character(unlist(result$data)), collapse = "\n")
    lines <- strsplit(text, "\r?\n")[[1]]
    lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
    if (!length(lines)) {
        return(with_provenance(tibble::tibble(), client, result))
    }
    cells <- strsplit(lines, "\t", fixed = TRUE)
    header <- cells[[1]]
    rows <- lapply(cells[-1], function(r) {
        r <- c(r, rep("", max(0L, length(header) - length(r))))
        r[seq_along(header)]
    })
    data <- lapply(seq_along(header), function(i)
        vapply(rows, `[[`, "", i))
    names(data) <- header
    with_provenance(tibble::as_tibble(data, .name_repair = "minimal"),
        client, result)
}

job_tree <- function(job, phylo, client) {
    if (is.null(job$result_newick)) job <- job_get(job, client = client)
    meta <- job$result_metadata %||% list()
    meta$provenance <- NULL
    plastics <- as.character(unlist(meta$columns))
    organisms <- names(meta$data %||% list())
    matrix <- matrix(FALSE, nrow = length(organisms), ncol = length(plastics),
        dimnames = list(organisms, plastics))
    for (org in organisms) {
        cells <- meta$data[[org]]
        for (p in plastics) {
            matrix[org, p] <- identical(cells[[p]], "yes")
        }
    }
    newick <- job$result_newick
    tree <- NULL
    if (isTRUE(phylo) && !is.null(newick) &&
        requireNamespace("ape", quietly = TRUE)) {
        tree <- ape::read.tree(text = newick)
    }
    counts <- meta[setdiff(names(meta), c("columns", "data"))]
    out <- c(list(newick = newick, matrix = matrix, phylo = tree), counts)
    out <- structure(out, class = "plasticdb_tree")
    job_result_provenance(out, job, client)
}

#' @export
print.plasticdb_tree <- function(x, ...) {
    cat(sprintf("<plasticdb_tree: %d organisms x %d plastics>\n",
        nrow(x$matrix), ncol(x$matrix)))
    if (!is.null(x$newick)) {
        text <- x$newick
        if (nchar(text) > 70) text <- paste0(substr(text, 1, 67), "...")
        cat("  newick: ", text, "\n", sep = "")
    }
    invisible(x)
}

view_params <- function(scov, qcov, bitscore, all_hits) {
    list(scov = scov, qcov = qcov, bitscore = bitscore,
        all = if (isTRUE(all_hits)) TRUE)
}

#' @rdname job_results
#' @export
job_comparison <- function(job, scov = NULL, qcov = NULL, bitscore = NULL,
                            all_hits = FALSE, client = plasticdb_client()) {
    endpoint <- job_endpoint(job, "jobs_comparison", "shared_comparison")
    params <- c(list(format = "json"), view_params(scov, qcov, bitscore,
        all_hits))
    result <- pdb_request(client, endpoint, params, job_ref(job))
    matrix <- result$data$comparison %||% list()
    out <- matrix[setdiff(names(matrix), "genomes")]
    out$plastics <- as.character(unlist(out$plastics))
    out$genomes <- records_to_tibble(matrix$genomes %||% list())
    out$view <- result$data$view
    as_record(out, client, result)
}

#' @rdname job_results
#' @export
job_download <- function(job, path = ".", scov = NULL, qcov = NULL,
                            bitscore = NULL, all_hits = FALSE,
                            client = plasticdb_client()) {
    job <- as_job(job, client)
    expired <- job$results_expired_at
    if (!is.null(expired)) {
        pdb_abort("results_expired", c(sprintf(paste("Job %s's result files",
            "were deleted on %s under the retention policy."), job$id,
            substr(expired, 1, 10)), i = paste("Its hits are still",
            "readable: job_results()")),
            results_expired_at = parse_time(expired), status = 410L,
            reason = "expired")
    }
    endpoint <- job_endpoint(job, "jobs_download", "shared_download")
    pdb_download(client, endpoint, path,
        params = view_params(scov, qcov, bitscore, all_hits),
        path_params = job_ref(job), default_name = sprintf(
            "plasticdb_%s_%s.tsv", job$job_type, substr(job$id, 1, 8)))
}

owner_only <- function(job, client, what = "Share links") {
    if (!is.null(job_token(job))) {
        pdb_abort("permission_denied",
            "A job opened from a share link is read-only.", status = 403L)
    }
    require_api_key(client, what)
}

#' @rdname job_results
#' @export
job_share <- function(job, expires_in_days = NULL, idempotency_key = NULL,
                        client = plasticdb_client()) {
    client <- check_client(client)
    owner_only(job, client)
    body <- NULL
    if (!is.null(expires_in_days)) {
        if (!is.numeric(expires_in_days) || length(expires_in_days) != 1L ||
            expires_in_days < 1 || expires_in_days > 365) {
            arg_error("`expires_in_days` must be between 1 and 365.")
        }
        body <- list(json = list(expires_in_days = as.integer(
            expires_in_days)))
    }
    result <- pdb_request(client, ENDPOINTS$jobs_share_create,
        path_params = list(job_id = job_id_of(job)), body = body,
        idempotency_key = idempotency_key)
    data <- result$data
    data$url <- paste0(client$root_url, data$path)
    as_record(data, client, result)
}

#' @rdname job_results
#' @export
job_shares <- function(job, client = plasticdb_client()) {
    client <- check_client(client)
    owner_only(job, client)
    result <- pdb_request(client, ENDPOINTS$jobs_shares,
        path_params = list(job_id = job_id_of(job)))
    with_provenance(records_to_tibble(result$data %||% list()), client,
        result)
}

#' @rdname job_results
#' @export
job_unshare <- function(job, share, client = plasticdb_client()) {
    client <- check_client(client)
    owner_only(job, client)
    id <- if (is.list(share)) share$id else share
    if (length(id) != 1L || !grepl("^[0-9]+$", as.character(id))) {
        arg_error("`share` must be one share link or its numeric id.")
    }
    pdb_request(client, ENDPOINTS$jobs_share_revoke,
        path_params = list(job_id = job_id_of(job),
            share_id = as.integer(id)))
    invisible(TRUE)
}

#' @rdname job_results
#' @export
job_shared <- function(token_or_url, client = plasticdb_client()) {
    if (!is.character(token_or_url) || length(token_or_url) != 1L ||
        is.na(token_or_url)) {
        arg_error("`token_or_url` must be a share link or token.")
    }
    token <- sub("[?#].*$", "", trimws(token_or_url))
    token <- sub("/+$", "", token)
    token <- utils::tail(strsplit(token, "/", fixed = TRUE)[[1]], 1)
    if (!length(token) || !nzchar(token)) {
        arg_error("No share token given.")
    }
    result <- pdb_request(client, ENDPOINTS$shared_get, LEAN,
        list(token = token))
    new_job(result$data, result, token = token)
}

#' @rdname job_results
#' @export
jobs_list <- function(limit = Inf, client = plasticdb_client()) {
    client <- check_client(client)
    require_api_key(client, "Listing jobs")
    list_tibble(client, ENDPOINTS$jobs_list, limit = limit, kind = "job")
}

#' @rdname job_results
#' @export
job_provenance <- function(job, client = plasticdb_client()) {
    job <- as_job(job, client)
    meta <- attr(job, "plasticdb")
    record <- job$result_metadata$provenance
    record <- if (is_object(record)) record else list()
    tools <- job$tool_versions
    list(job_id = job$id, job_type = job$job_type,
        release = job$release %||% meta$release,
        release_source = job$release_source %||% meta$release_source,
        reference_sha256 = job$reference_sha256,
        tool_versions = if (length(tools)) unlist(tools),
        app_version = if (is.character(record$app_version))
            record$app_version,
        record = record)
}

#' @rdname job_results
#' @export
job_replayed <- function(job) {
    isTRUE(attr(job, "plasticdb")$replayed)
}
