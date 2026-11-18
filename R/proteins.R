MAX_MERGE_HOPS <- 5L

warn_merged <- function(old, new) {
    rlang::warn(sprintf("PlasticDB: %s was merged into %s; returning %s.",
        old, new, new), class = "plasticdb_merged_warning",
        merged_from = old, merged_into = new)
}

#' Proteins
#'
#' Wild-type proteins and engineered variants linked to plastic degradation,
#' with the evidence behind each link.
#'
#' * `proteins_list()` lists proteins matching the filters in `...` (wild
#'   types and variants; `kind =` narrows). Like every entity list, only
#'   records with a positive result are listed unless `polarity =` or
#'   `include_negative = TRUE` says otherwise.
#' * `proteins_get()` returns one protein by accession (`PB...`, any case)
#'   or numeric id. An engineered variant's accession returns the variant, as
#'   [variants_get()] does. A merged accession signals `plasticdb_merged`
#'   naming the survivor (`merged_into`); with `follow_merges = TRUE` the
#'   survivor is returned instead, with a `plasticdb_merged_warning`. A
#'   withdrawn record signals `plasticdb_gone` (`reason` `"withdrawn"`).
#'   The per-protein tables of a merged accession signal `plasticdb_merged`
#'   too (its `location` is the survivor's table); given a variant's
#'   accession they signal `plasticdb_engineered_variant`, since variants
#'   have none (its record carries them: [variants_get()]).
#' * `proteins_interactions()` is the protein's evidence spine: `"direct"`
#'   (assayed on it) or `"indirect"` (via its organism) interactions, as one
#'   row per interaction, experiment or observation (`level`).
#' * `proteins_kinetics()` and `proteins_properties()` are the kinetic
#'   parameters and stability/activity properties of a protein and its
#'   variants; `proteins_variants()` its engineered variants, with sequences.
#' * `proteins_sequences()` lists proteins and returns their sequences as an
#'   [Biostrings::AAStringSet] (names are accessions, the list columns are in
#'   `mcols()`).
#' * `proteins_fasta()` and `proteins_structure()` are one protein's FASTA
#'   record and primary structure (PDB text, or a file with `path`);
#'   `proteins_lineage()` a wild type with every variant engineered from it;
#'   `proteins_engineered_parents()` the wild types that have variants. A
#'   variant's accession gives the variant's FASTA record or structure. These
#'   four are live only: a pinned client gets `plasticdb_release_live_only`
#'   (for a pinned structure use [downloads_structure()]).
#'
#' Filters are the API's own query parameters, with the same meaning on every
#' function: `plastic`, `include_subtypes`, `microorganism`, `polarity`,
#' `include_negative`, `evidence_method`, `min_year`, `max_year`,
#' `updated_since`, `sort_by`, `sort_order` and so on. An unknown name
#' signals `plasticdb_argument_error` naming the valid ones; an unknown value
#' is a server 422 (`plasticdb_validation`).
#'
#' Tables have one flat row per record: nested objects become `parent.child`
#' columns (`source_microorganism.name`, `paper.doi`), lists of plain values
#' become list columns, and per-plastic evidence becomes `plastics_positive`,
#' `plastics_negative` and `plastics_inconclusive`.
#'
#' @param ... Filters (see Details), named as the API names them.
#' @param accession,ref A protein accession or numeric id.
#' @param view `"full"` or `"summary"` (leaves out the per-record evidence
#'   lists, much smaller for well-studied enzymes).
#' @param include_sequenceless Include records without a sequence.
#' @param follow_merges Return the survivor of a merged accession.
#' @param kind `"all"`, `"direct"` or `"indirect"` interactions.
#' @param level One row per `"interaction"`, `"experiment"` or
#'   `"observation"`. Deeper levels repeat the interaction's columns and add
#'   `experiment.*` (and `observation.*`) columns.
#' @param limit The most records to return (`Inf` for all of them).
#' @param path For `proteins_structure()`, a file or directory to write to.
#' @param updated_since Only records changed since this time.
#' @param client A [plasticdb_client()].
#'
#' @return Lists return a [tibble::tibble()] with the provenance (release,
#'   source, URL, total) in `attr(x, "plasticdb")`, see [provenance()].
#'   Single records return a `plasticdb_record` list with the API's nesting.
#'   `proteins_sequences()` returns an [Biostrings::AAStringSet],
#'   `proteins_fasta()` a string.
#' @seealso [variants_list()], [evidence_interactions()], [as_aastringset()]
#' @name proteins
#' @examples
#' pet <- proteins_list(plastic = "PET", kind = "wild_type", limit = 5)
#' pet[, c("accession", "name", "source_microorganism.name")]
#' provenance(pet)$total
#' \donttest{
#' petase <- proteins_get("PBFCTW", view = "summary")
#' petase$name
#' proteins_interactions("PBFCTW", level = "observation", limit = 3)
#' proteins_sequences(plastic = "PHB", kind = "wild_type", min_year = 2025)
#' }
NULL

#' @rdname proteins
#' @export
proteins_list <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), PROTEIN_LIST_FILTERS, "proteins_list")
    list_tibble(client, ENDPOINTS$proteins_list, params, limit = limit,
        kind = "protein")
}

#' @rdname proteins
#' @export
proteins_get <- function(accession, view = c("full", "summary"),
                            include_sequenceless = FALSE, follow_merges = FALSE,
                            client = plasticdb_client()) {
    view <- match.arg(view)
    params <- list(view = view,
        include_sequenceless = if (isTRUE(include_sequenceless)) TRUE)
    ref <- accession
    for (i in seq_len(MAX_MERGE_HOPS)) {
        out <- tryCatch(
            get_record(client, ENDPOINTS$proteins_get, params,
                list(accession = ref)),
            plasticdb_engineered_variant = function(e) {
                variants_get(e$accession,
                    include_sequenceless = include_sequenceless,
                    follow_merges = follow_merges, client = client)
            },
            plasticdb_merged = function(e) {
                if (!isTRUE(follow_merges)) rlang::cnd_signal(e)
                warn_merged(ref, e$merged_into)
                if (identical(e$kind, "engineered")) {
                    return(variants_get(e$merged_into,
                        include_sequenceless = include_sequenceless,
                        client = client))
                }
                structure(list(), next_ref = e$merged_into)
            })
        if (is.null(attr(out, "next_ref"))) {
            return(out)
        }
        ref <- attr(out, "next_ref")
    }
    pdb_abort("unexpected_response", sprintf("%s: more than %d merges",
        accession, MAX_MERGE_HOPS))
}

sub_list <- function(client, endpoint, accession, params = list(),
                        limit = Inf, kind = NULL, level = "interaction") {
    list_tibble(client, endpoint, params, list(accession = accession),
        limit = limit, kind = kind, level = level)
}

#' @rdname proteins
#' @export
proteins_interactions <- function(accession,
                                    kind = c("all", "direct", "indirect"),
                                    level = c("interaction", "experiment",
                                        "observation"),
                                    include_sequenceless = FALSE, limit = Inf,
                                    client = plasticdb_client()) {
    kind <- match.arg(kind)
    level <- match.arg(level)
    sub_list(client, ENDPOINTS$proteins_interactions, accession,
        list(kind = kind, include_sequenceless =
            if (isTRUE(include_sequenceless)) TRUE),
        limit = limit, kind = "interaction", level = level)
}

#' @rdname proteins
#' @export
proteins_kinetics <- function(accession, limit = Inf,
                                client = plasticdb_client()) {
    sub_list(client, ENDPOINTS$proteins_kinetics, accession, limit = limit)
}

#' @rdname proteins
#' @export
proteins_properties <- function(accession, limit = Inf,
                                client = plasticdb_client()) {
    sub_list(client, ENDPOINTS$proteins_properties, accession, limit = limit)
}

#' @rdname proteins
#' @export
proteins_variants <- function(accession, include_sequenceless = FALSE,
                                limit = Inf, client = plasticdb_client()) {
    sub_list(client, ENDPOINTS$proteins_variants, accession,
        list(include_sequenceless = if (isTRUE(include_sequenceless)) TRUE),
        limit = limit, kind = "variant")
}

#' @rdname proteins
#' @export
proteins_sequences <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), PROTEIN_LIST_FILTERS,
        "proteins_sequences")
    protein_sequences(client, params, limit)
}

#' @rdname proteins
#' @export
proteins_fasta <- function(ref, client = plasticdb_client()) {
    tryCatch(body_text(pdb_request(client, ENDPOINTS$proteins_fasta,
        path_params = list(ref = ref))$data),
        plasticdb_engineered_variant = function(e)
            variants_fasta(e$accession, client = client))
}

#' @rdname proteins
#' @export
proteins_structure <- function(ref, path = NULL, client = plasticdb_client()) {
    tryCatch(structure_text(client, ENDPOINTS$proteins_structure, ref, path),
        plasticdb_engineered_variant = function(e)
            variants_structure(e$accession, path = path, client = client))
}

#' @rdname proteins
#' @export
proteins_lineage <- function(accession, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$engineering_get,
        path_params = list(accession = accession))
}

#' @rdname proteins
#' @export
proteins_engineered_parents <- function(updated_since = NULL, limit = Inf,
                                        client = plasticdb_client()) {
    list_tibble(client, ENDPOINTS$engineering_list,
        list(updated_since = updated_since), limit = limit, kind = "protein")
}

structure_text <- function(client, endpoint, ref, path) {
    if (!is.null(path)) {
        return(pdb_download(client, endpoint, path,
            path_params = list(ref = ref), default_name = paste0(ref, ".pdb")))
    }
    body_text(pdb_request(client, endpoint, path_params = list(ref = ref))$data)
}

#' Engineered variants
#'
#' Engineered variants of wild-type proteins: their mutations, engineering
#' method and evidence. Records, FASTA and lists answer a pinned release too;
#' only the structure is live only (a pinned client gets
#' `plasticdb_release_live_only`).
#'
#' * `variants_list()` lists variants matching the filters in `...`
#'   (`proteins_list(kind = "engineered")`; see [proteins] for the filters).
#' * `variants_get()` returns one variant by accession or numeric id. Merges
#'   behave as in [proteins_get()]; a withdrawn variant (also one whose parent
#'   protein was withdrawn) signals `plasticdb_gone`. On a past release the
#'   record comes from its snapshot: a variant the release did not contain is
#'   `plasticdb_not_found` (merges are not followed there),
#'   `include_sequenceless = TRUE` is `plasticdb_release_unsupported_filter`,
#'   and the fields it cannot fill are in
#'   `last_response()$unavailable_fields`.
#' * `variants_of()` lists the variants of one parent protein, with
#'   sequences (the same as [proteins_variants()]).
#' * `variants_sequences()` returns the listed variants' sequences as an
#'   [Biostrings::AAStringSet].
#' * `variants_fasta()` and `variants_structure()` are one variant's FASTA
#'   record and structure coordinates.
#'
#' @inheritParams proteins
#' @param ref A variant accession or numeric id.
#' @param parent The parent protein's accession.
#'
#' @return As for [proteins]: tibbles with provenance, `plasticdb_record`
#'   lists, an [Biostrings::AAStringSet], or text.
#' @name variants
#' @examples
#' variants_of("PBFCTW", limit = 3)[, c("accession", "variant_name")]
#' \donttest{
#' variants_get("PBDTZX")$variant_name
#' variants_sequences(plastic = "PET", limit = 5)
#' }
NULL

#' @rdname variants
#' @export
variants_list <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), VARIANT_LIST_FILTERS, "variants_list")
    params$kind <- "engineered"
    list_tibble(client, ENDPOINTS$proteins_list, params, limit = limit,
        kind = "protein")
}

#' @rdname variants
#' @export
variants_get <- function(ref, include_sequenceless = FALSE,
                            follow_merges = FALSE,
                                client = plasticdb_client()) {
    params <- list(include_sequenceless =
        if (isTRUE(include_sequenceless)) TRUE)
    current <- ref
    for (i in seq_len(MAX_MERGE_HOPS)) {
        out <- tryCatch(
            get_record(client, ENDPOINTS$variants_get, params,
                list(ref = current)),
            plasticdb_merged = function(e) {
                if (!isTRUE(follow_merges)) rlang::cnd_signal(e)
                warn_merged(as.character(current), e$merged_into)
                structure(list(), next_ref = e$merged_into)
            })
        if (is.null(attr(out, "next_ref"))) {
            return(out)
        }
        current <- attr(out, "next_ref")
    }
    pdb_abort("unexpected_response", sprintf("%s: more than %d merges", ref,
        MAX_MERGE_HOPS))
}

#' @rdname variants
#' @export
variants_of <- function(parent, limit = Inf, client = plasticdb_client()) {
    proteins_variants(parent, limit = limit, client = client)
}

#' @rdname variants
#' @export
variants_sequences <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), VARIANT_LIST_FILTERS,
        "variants_sequences")
    params$kind <- "engineered"
    protein_sequences(client, params, limit)
}

#' @rdname variants
#' @export
variants_fasta <- function(ref, client = plasticdb_client()) {
    body_text(pdb_request(client, ENDPOINTS$variants_fasta,
        path_params = list(ref = ref))$data)
}

#' @rdname variants
#' @export
variants_structure <- function(ref, path = NULL, client = plasticdb_client()) {
    structure_text(client, ENDPOINTS$variants_structure, ref, path)
}
