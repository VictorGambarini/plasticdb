# Jobs, against the synthetic fixtures in fixtures/synthetic
# (tools/make-job-fixtures.R): jobs cannot be run live without an API key.

header_of <- function(req, name) {
    headers <- req$headers
    headers[[match(tolower(name), tolower(names(headers)))]]
}

fasta_text <- ">q1 test\nMKVLAAGIVALLLAAGCSSSPAAQ\n"

test_that("a job needs an API key, checked before anything is sent", {
    local_clean_env()
    calls <- local_job_api()
    err <- expect_error(annotate_gene(fasta_text),
        class = "plasticdb_api_key_required")
    expect_s3_class(err, "plasticdb_authentication")
    expect_match(conditionMessage(err), "PLASTICDB_API_KEY")
    expect_error(jobs_list(), class = "plasticdb_api_key_required")
    expect_error(build_tree(plastic = "PET"),
        class = "plasticdb_api_key_required")
    expect_error(search_sequence("MKVLAAGIVA"),
        class = "plasticdb_api_key_required")
    expect_length(calls$requests, 0)
})

test_that("submit_job() checks the type and its parameters first", {
    local_clean_env()
    client <- key_client()
    expect_error(submit_job("nope", fasta_text, client = client),
        class = "plasticdb_argument_error")
    expect_error(submit_job("annotate_gene", fasta_text, genus_column = 1,
        client = client), class = "plasticdb_argument_error")
    expect_error(submit_job("search-sequence", "MKV", client = client),
        class = "plasticdb_argument_error")
    expect_error(submit_job("annotate_gene", client = client),
        class = "plasticdb_argument_error")
    expect_error(submit_job("build_tree", fasta_text, client = client),
        class = "plasticdb_argument_error")
})

test_that("a submission is multipart, keyed and pinned", {
    local_clean_env()
    calls <- local_job_api()
    client <- key_client(release = "2026.30")
    job <- annotate_genome(fasta_text, evalue = 1e-10, include_variants = TRUE,
        min_scov = 50, client = client)
    expect_s3_class(job, "plasticdb_job")
    expect_identical(job$id, "done1")
    expect_identical(job$status, "waiting")
    expect_false(job_replayed(job))
    req <- calls$requests[[1]]
    expect_identical(req$method, "POST")
    expect_identical(query_of(req$url)$release, "2026.30")
    expect_match(header_of(req, "Idempotency-Key"),
        "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")
    expect_true("Authorization" %in% names(req$headers))
    fields <- form_fields(req)
    expect_identical(fields$job_type, "annotate_genome")
    expect_identical(fields$evalue, "0.0000000001")
    expect_identical(fields$include_variants, "true")
    expect_identical(fields$min_scov, "50")
    expect_identical(fields$file$name, "input.fasta")
    expect_identical(fields$file$type, "text/plain")
    expect_false("blast_type" %in% names(fields))
})

test_that("an explicit release and idempotency key are sent as given", {
    local_clean_env()
    calls <- local_job_api()
    annotate_gene(fasta_text, release = "2026.40", idempotency_key = "run-1",
        client = key_client(release = "2026.30"))
    req <- calls$requests[[1]]
    expect_identical(query_of(req$url)$release, "2026.40")
    expect_identical(header_of(req, "Idempotency-Key"), "run-1")
})

test_that("inputs: paths, gzip, Biostrings sets, residues and tables", {
    local_clean_env()
    calls <- local_job_api()
    client <- key_client()
    dir <- withr::local_tempdir()

    gz <- file.path(dir, "genome.faa.gz")
    con <- gzfile(gz, "w")
    writeLines(c(">a", "MKVLA"), con)
    close(con)
    annotate_genome(gz, client = client)
    file <- form_fields(calls$requests[[1]])$file
    expect_identical(file$name, "genome.fasta.gz")
    expect_identical(file$type, "application/gzip")
    expect_identical(normalizePath(file$path), normalizePath(gz))

    # gzip bytes under a plain name are still named .gz
    mislabelled <- file.path(dir, "proteins.txt")
    file.copy(gz, mislabelled)
    annotate_gene(mislabelled, client = client)
    expect_identical(form_fields(calls$requests[[2]])$file$name,
        "proteins.fasta.gz")

    aa <- Biostrings::AAStringSet(c(p1 = "MKVLAAG", p2 = "MSTNPYQR"))
    seen <- NULL
    calls2 <- local_job_api(function(req) {
        f <- form_fields(req)$file
        seen <<- readLines(f$path)
        NULL
    })
    hmm_screen(aa, target_polymers = c("PET", "PLA"),
        threshold_mode = "gathering", client = client)
    req <- calls2$requests[[1]]
    expect_identical(seen, c(">p1", "MKVLAAG", ">p2", "MSTNPYQR"))
    expect_identical(names(req$body$data)[names(req$body$data) ==
        "target_polymers"], c("target_polymers", "target_polymers"))
    expect_identical(unname(unlist(req$body$data[names(req$body$data) ==
        "target_polymers"])), c("PET", "PLA"))

    annotate_gene("MKVLAAGIVALLL", client = client)
    expect_identical(seen, c(">query", "MKVLAAGIVALLL"))
    expect_error(annotate_gene("not-a-file.faa", client = client),
        class = "plasticdb_argument_error")

    taxa <- data.frame(Sample = c("s1", "s2"), Genus = c("Ideonella", NA),
        Species = c("sakaiensis", "coli"))
    annotate_list(taxa, genus_column = 2, species_column = 3,
        field_separator = "tab", client = client)
    req <- calls2$requests[[length(calls2$requests)]]
    fields <- form_fields(req)
    expect_identical(fields$file$name, "input.tsv")
    expect_identical(fields$file$type, "text/tab-separated-values")
    expect_identical(fields$genus_column, "2")
    expect_identical(seen, c("Sample\tGenus\tSpecies",
        "s1\tIdeonella\tsakaiensis", "s2\t\tcoli"))
    expect_error(annotate_list(taxa, field_separator = "pipe",
        client = client))
})

test_that("compare_genomes() labels each genome by its upload's name", {
    local_clean_env()
    calls <- local_job_api()
    client <- key_client()
    dna <- Biostrings::DNAStringSet(c(c1 = "ATGAAAGTG"))
    compare_genomes(list(`E. coli K12` = fasta_text, Isak = dna),
        blast_type = "blastx", client = client)
    data <- calls$requests[[1]]$body$data
    files <- data[names(data) == "files"]
    expect_identical(vapply(files, `[[`, "", "name"),
        c(files = "E._coli_K12.fasta", files = "Isak.fasta"))
    compare_genomes(c(fasta_text, fasta_text), labels = c("a", "b"),
        client = client)
    expect_error(compare_genomes(list(fasta_text), client = client),
        class = "plasticdb_argument_error")
    expect_error(compare_genomes(list(x = fasta_text, x = fasta_text),
        client = client), class = "plasticdb_argument_error")
    expect_error(compare_genomes(list(fasta_text, fasta_text),
        labels = "one", client = client), class = "plasticdb_argument_error")
})

test_that("pathway_analysis() sends target_plastic", {
    local_clean_env()
    calls <- local_job_api()
    pathway_analysis(fasta_text, target_plastic = "PLA",
        min_step_identity = 50, client = key_client())
    fields <- form_fields(calls$requests[[1]])
    expect_identical(fields$target_plastic, "PLA")
    expect_identical(fields$min_step_identity, "50")
})

test_that("a keyed submission is retried with the same key", {
    local_clean_env()
    local_no_sleep()
    n <- 0
    calls <- local_job_api(function(req) {
        n <<- n + 1
        if (n == 1) {
            return(json_response(list(detail = "upstream"), status = 502L))
        }
        resp <- source(test_path("fixtures", "synthetic",
            "jobs-POST.R"))$value
        resp$headers[["idempotent-replayed"]] <- "true"
        resp
    })
    job <- annotate_gene(fasta_text, client = key_client())
    keys <- vapply(calls$requests, header_of, "", name = "Idempotency-Key")
    expect_length(keys, 2)
    expect_identical(keys[[1]], keys[[2]])
    expect_true(job_replayed(job))
})

test_that("without a key a submission is not resent after a 5xx", {
    local_clean_env()
    local_no_sleep()
    calls <- local_job_api(function(req) json_response(list(detail = "boom"),
        status = 502L))
    client <- key_client(max_retries = 3)
    # The policy itself: an anonymous keyed POST is not retry-safe.
    anon <- plasticdb_client(cache = FALSE)
    expect_false(plasticdb:::retry_safe_for(anon,
        plasticdb:::ENDPOINTS$jobs_submit))
    expect_true(plasticdb:::retry_safe_for(client,
        plasticdb:::ENDPOINTS$jobs_submit))
    expect_true(plasticdb:::retry_safe_for(anon,
        plasticdb:::ENDPOINTS$search_sequence))
    req <- plasticdb:::build_httr2(client, "POST",
        "https://v2.plasticdb.org/api/v1/jobs")
    resp <- plasticdb:::send_request(client, req, retry_safe = FALSE)
    expect_identical(httr2::resp_status(resp), 502L)
    expect_length(calls$requests, 1)
})

test_that("when retries run out, the error names the key", {
    local_clean_env()
    local_no_sleep()
    calls <- local_job_api(function(req) json_response(list(detail = "boom"),
        status = 503L))
    err <- expect_error(annotate_gene(fasta_text, idempotency_key = "k-42",
        client = key_client(max_retries = 1)),
        class = "plasticdb_server_error")
    expect_identical(err$idempotency_key, "k-42")
    expect_match(conditionMessage(err), "idempotency_key = \"k-42\"",
        fixed = TRUE)
    expect_length(calls$requests, 2)
})

test_that("submission errors keep their classes", {
    local_clean_env()
    local_api(function(req) json_response(list(detail = list(list(
        type = "idempotency_key_reused", loc = list("header",
            "Idempotency-Key"), msg = "different request"))), status = 422L))
    expect_error(annotate_gene(fasta_text, client = key_client()),
        class = "plasticdb_idempotency_key_reused")
    local_api(function(req) json_response(list(detail = "too big"),
        status = 413L))
    expect_error(annotate_gene(fasta_text, client = key_client()),
        class = "plasticdb_payload_too_large")
    local_api(function(req) json_response(list(detail = list(list(
        type = "release_unsupported", loc = list("query", "release"),
        msg = "no HMM library"))), status = 422L))
    expect_error(hmm_screen(fasta_text, client = key_client(
        release = "2026.30")), class = "plasticdb_release_unsupported_filter")
})

test_that("job_get() and job_status() read the job without its hits", {
    local_clean_env()
    calls <- local_job_api()
    client <- key_client(release = "2026.99")
    job <- job_get("done1", client = client)
    expect_identical(job$status, "done")
    expect_identical(query_of(calls$requests[[1]]$url),
        list(include = "none"))
    expect_identical(job_status("run1", client = client), "running")
    expect_output(print(job), "done1 annotate_genome done release=2026.30")
})

test_that("wait_job() polls until done, backing off", {
    local_clean_env()
    sleeps <- local_no_sleep()
    n <- 0
    local_job_api(function(req) {
        n <<- n + 1
        file <- if (n < 3) "run1-18132b.R" else "done1-18132b.R"
        source(test_path("fixtures", "synthetic", "jobs", file))$value
    })
    job <- wait_job("done1", poll_interval = 2, client = key_client())
    expect_identical(job$status, "done")
    expect_identical(sleeps$waits, c(2, 3))
})

test_that("a finished job is returned without a poll", {
    local_clean_env()
    calls <- local_job_api()
    client <- key_client()
    job <- job_get("done1", client = client)
    again <- wait_job(job, client = client)
    expect_length(calls$requests, 1)
    expect_identical(again$id, "done1")
})

test_that("a failed job signals job_failed, or is returned", {
    local_clean_env()
    local_job_api()
    client <- key_client()
    err <- expect_error(wait_job("err1", client = client),
        class = "plasticdb_job_failed")
    expect_s3_class(err, "plasticdb_job_error")
    expect_match(conditionMessage(err), "blastx")
    expect_identical(err$job$id, "err1")
    job <- wait_job("err1", error = FALSE, client = client)
    expect_identical(job$status, "error")
    expect_error(job_results(job, client = client),
        class = "plasticdb_job_failed")
})

test_that("wait_job() times out and leaves the job running", {
    local_clean_env()
    local_job_api()
    clock <- as.POSIXct("2026-11-18 00:00:00", tz = "UTC")
    testthat::local_mocked_bindings(
        pdb_now = function() clock,
        pdb_sleep = function(seconds) clock <<- clock + seconds,
        .package = "plasticdb")
    err <- expect_error(wait_job("run1", timeout = 10, poll_interval = 4,
        client = key_client()), class = "plasticdb_job_timeout")
    expect_identical(err$job$status, "running")
    expect_match(conditionMessage(err), "keeps running")
})

test_that("wait_job() waits out a 429", {
    local_clean_env()
    sleeps <- local_no_sleep()
    n <- 0
    local_job_api(function(req) {
        n <<- n + 1
        if (n == 1) {
            return(json_response(list(detail = "slow"), status = 429L,
                headers = list(`retry-after` = "90")))
        }
        NULL
    })
    job <- wait_job("done1", client = key_client(max_retry_wait = 5))
    expect_identical(job$status, "done")
    expect_identical(sleeps$waits, 90)
})

test_that("job_results() pages the hits of a search job", {
    local_clean_env()
    calls <- local_job_api()
    client <- key_client()
    hits <- job_results("done1", client = client)
    expect_identical(nrow(hits), 2L)
    expect_true(all(c("query_seq_id", "subject_accession", "pident",
        "protein.accession", "protein.source_microorganism.name",
        "plastics_positive", "plastics_negative") %in% names(hits)))
    expect_false(any(grepl("degradation_links|^evidence$", names(hits))))
    expect_identical(hits$plastics_positive[[1]], "PET")
    expect_identical(hits$plastics_negative[[1]], "PE")
    expect_identical(provenance(hits)$release, "2026.30")
    expect_identical(provenance(hits)$total, 2)
    urls <- vapply(calls$requests, `[[`, "", "url")
    expect_match(urls[[2]], "/jobs/done1/results\\?limit=500&offset=0$")
})

test_that("job_results() of a running job asks to wait first", {
    local_clean_env()
    local_job_api()
    err <- expect_error(job_results("run1", client = key_client()),
        class = "plasticdb_job_error")
    expect_match(conditionMessage(err), "wait_job")
})

test_that("compare_genomes results: hits by genome and the matrix", {
    local_clean_env()
    local_job_api()
    client <- key_client()
    hits <- job_results("cmp1", client = client)
    expect_identical(hits$source_label, c("E_coli_K12", "I_sakaiensis"))
    cmp <- job_comparison("cmp1", client = client)
    expect_identical(cmp$plastics, c("PE", "PET"))
    expect_identical(cmp$genomes$label, c("E_coli_K12", "I_sakaiensis"))
    expect_true(all(c("counts.PET", "normalised.PET") %in%
        names(cmp$genomes)))
    expect_identical(cmp$view$min_scov, 70L)
})

test_that("hmm_screen, pathway and annotate_list results", {
    local_clean_env()
    local_job_api()
    client <- key_client()
    hmm <- job_results("hmm1", client = client)
    expect_identical(hmm$calibration_status, c("separable", "separable"))
    # Alignment fields are NA on HMM hits, never 0.
    expect_true(all(is.na(hmm$pident) & is.na(hmm$length) &
        is.na(hmm$qcov) & is.na(hmm$scov)))
    expect_identical(hmm$profile_accession, c("PF12740.1", NA))
    expect_identical(hmm$bitscore, c(412.6, 539.2))
    expect_identical(hmm$domain_score, c(410.9, NA))
    expect_identical(hmm$domain_c_evalue, c(2.1e-124, NA))
    expect_identical(hmm$domain_i_evalue, c(3.4e-124, NA))
    expect_identical(hmm$hmm_from, c(3L, NA))
    expect_identical(hmm$hmm_to, c(262L, NA))
    expect_identical(hmm$env_from, c(24L, NA))
    expect_identical(hmm$env_to, c(285L, NA))
    steps <- job_results("pwy1", client = client)
    expect_identical(steps$order, c(1L, 2L))
    expect_identical(steps$present, c(TRUE, FALSE))
    expect_identical(steps$best_hit.subject_label, c("IsPETase", NA))
    expect_identical(provenance(steps)$release, "2026.30")
    taxa <- job_results("taxa1", client = client)
    expect_identical(names(taxa), c("Sample", "Genus", "Species",
        "PlasticDB_match", "PlasticDB_plastics"))
    expect_identical(taxa$PlasticDB_match, c("Ideonella sakaiensis", ""))
})

test_that("a tree job gives Newick text and the organism x plastic matrix", {
    local_clean_env()
    local_job_api()
    tree <- job_results("tree1", phylo = FALSE, client = key_client())
    expect_s3_class(tree, "plasticdb_tree")
    expect_identical(tree$newick,
        "((Ideonella_sakaiensis,Pseudomonas_putida)Pseudomonadota);")
    expect_identical(dim(tree$matrix), c(2L, 2L))
    expect_true(tree$matrix["Ideonella sakaiensis", "PET"])
    expect_false(tree$matrix["Ideonella sakaiensis", "PE"])
    expect_null(tree$phylo)
    expect_identical(tree$organism_count, 2L)
    expect_output(print(tree), "2 organisms x 2 plastics")
    skip_if_not_installed("ape")
    tree <- job_results("tree1", client = key_client())
    expect_s3_class(tree$phylo, "phylo")
    expect_setequal(tree$phylo$tip.label,
        c("Ideonella_sakaiensis", "Pseudomonas_putida"))
})

test_that("build_tree() posts JSON filters, keyed", {
    local_clean_env()
    calls <- local_job_api()
    job <- build_tree(plastic = "PET", class_ = "Gammaproteobacteria",
        microorganism_ids = 5, release = "2026.30", client = key_client())
    expect_identical(job$job_type, "build_tree")
    req <- calls$requests[[1]]
    expect_identical(req$body$type, "json")
    body <- req$body$data
    expect_identical(body$class, "Gammaproteobacteria")
    expect_identical(body$microorganism_ids, list(5L))
    expect_identical(query_of(req$url)$release, "2026.30")
    expect_false(is.null(header_of(req, "Idempotency-Key")))
    expect_error(build_tree(nope = 1, client = key_client()),
        class = "plasticdb_argument_error")
})

test_that("job_download() saves the results file", {
    local_clean_env()
    local_job_api()
    dir <- withr::local_tempdir()
    path <- job_download("done1", dir, client = key_client())
    expect_identical(basename(path), "plasticdb_annotate_genome_done1.tsv")
    expect_match(readLines(path)[[1]], "^# PlasticDB")
    calls <- local_job_api(function(req) {
        if (!grepl("/download", req$url)) return(NULL)
        httr2::response(status_code = 200L, headers = list(
            `content-type` = "text/tab-separated-values"),
            body = charToRaw("# all\n"))
    })
    job_download("done1", file.path(dir, "all.tsv"), all_hits = TRUE,
        scov = 0, client = key_client())
    q <- query_of(calls$requests[[2]]$url)
    expect_identical(q[c("all", "scov")], list(all = "true", scov = "0"))
})

test_that("expired result files are refused, by the job or the server", {
    local_clean_env()
    local_job_api()
    client <- key_client()
    job <- job_get("done1", client = client)
    job$results_expired_at <- "2026-11-01T00:00:00Z"
    err <- expect_error(job_download(job, client = client),
        class = "plasticdb_results_expired")
    expect_s3_class(err, "plasticdb_gone")
    expect_identical(err$reason, "expired")
    local_job_api(function(req) json_response(list(detail = "expired",
        reason = "expired", results_expired_at = "2026-11-01T00:00:00Z"),
        status = 410L))
    err <- expect_error(job_download(job_get("done1", client = client),
        withr::local_tempdir(), client = client),
        class = "plasticdb_results_expired")
    expect_s3_class(err$results_expired_at, "POSIXct")
})

test_that("share links: create (keyed), list, revoke, open", {
    local_clean_env()
    calls <- local_job_api()
    client <- key_client()
    link <- job_share("done1", expires_in_days = 30, client = client)
    expect_identical(link$token, "tok123")
    expect_identical(link$url, "https://v2.plasticdb.org/jobs/shared/tok123")
    req <- calls$requests[[1]]
    expect_identical(req$body$data, list(expires_in_days = 30L))
    expect_false(is.null(header_of(req, "Idempotency-Key")))
    shares <- job_shares("done1", client = client)
    expect_identical(shares$id, c(7L, 6L))
    expect_true(job_unshare("done1", shares[1, ], client = client))
    expect_identical(calls$requests[[3]]$method, "DELETE")
    expect_match(calls$requests[[3]]$url, "/jobs/done1/shares/7$")
    expect_error(job_share("done1", expires_in_days = 400, client = client),
        class = "plasticdb_argument_error")

    anon <- plasticdb_client(cache = FALSE)
    shared <- job_shared(link$url, client = anon)
    expect_output(print(shared), "(shared)", fixed = TRUE)
    hits <- job_results(shared, client = anon)
    expect_identical(nrow(hits), 2L)
    expect_match(calls$requests[[length(calls$requests)]]$url,
        "/jobs/shared/tok123/results")
    expect_error(job_share(shared, client = client),
        class = "plasticdb_permission_denied")
    expect_error(job_shares("done1", client = anon),
        class = "plasticdb_api_key_required")
})

test_that("jobs_list() and job_provenance()", {
    local_clean_env()
    local_job_api()
    client <- key_client()
    jobs <- jobs_list(client = client)
    expect_identical(provenance(jobs)$total, 2)
    expect_true(all(c("id", "job_type", "status") %in% names(jobs)))
    expect_false("result_metadata" %in% names(jobs))
    prov <- job_provenance("done1", client = client)
    expect_identical(prov$release, "2026.30")
    expect_identical(prov$release_source, "snapshot")
    expect_identical(prov$tool_versions, c(diamond = "2.1.9"))
    expect_identical(prov$app_version, "2.87.0")
    expect_match(prov$reference_sha256, "^[0-9a-f]{64}$")
})

test_that("search_sequence() is synchronous and retry-safe", {
    local_clean_env()
    calls <- local_job_api()
    res <- search_sequence(paste0(">one\n", "MKVLAAG\nIVALL"),
        include_variants = TRUE, client = key_client())
    expect_identical(res$hits$accession, "PBFCTW")
    expect_identical(res$diamond_version, "2.1.9")
    q <- query_of(calls$requests[[1]]$url)
    expect_identical(q$sequence, "MKVLAAGIVALL")
    expect_identical(q$include_variants, "true")
    expect_error(search_sequence(">a\nMK\n>b\nMV", client = key_client()),
        class = "plasticdb_argument_error")
    aa <- Biostrings::AAStringSet("MKVLA")
    expect_identical(plasticdb:::sequence_text(aa), "MKVLA")
})

test_that("safe_stem() matches the server's genome labels", {
    stem <- plasticdb:::safe_stem
    expect_identical(stem("genome.faa.gz"), "genome")
    expect_identical(stem("genome.fna"), "genome")
    expect_identical(stem("a b.c.fasta"), "a_b.c")
    expect_identical(stem("", 3L), "genome_3")
    expect_identical(stem("...", 2L), "genome_2")
    expect_identical(stem("x.tar.gz"), "x.tar")
})
