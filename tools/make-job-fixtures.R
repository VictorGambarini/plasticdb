# Write the synthetic job fixtures in tests/testthat/fixtures/synthetic.
# Run from the package root:
#
#     Rscript tools/make-job-fixtures.R [path/to/plasticdb-py/tests/fixtures]
#
# Jobs cannot be run against the live server without an API key, so these are
# not captured. The bodies are the Python client's job fixtures (plasticdb-py
# scripts/make_job_fixtures.py), which are built from the server's own schemas
# and worker code (JobRead / JobDetail, the result metadata the pipelines
# write) in the 2.87 shapes; tests/testthat/test-contract.R checks them
# against the vendored contract's schemas. Each scenario gets its own short
# job id so httptest2 can tell them apart by URL. The output is checked in.

args <- commandArgs(trailingOnly = TRUE)
src <- if (length(args)) args[[1]] else "../plasticdb-py/tests/fixtures"
out <- "tests/testthat/fixtures/synthetic"
stopifnot(dir.exists(src))
unlink(out, recursive = TRUE)

PY_JOB_ID <- "3f2c9a1e-7b4d-4e8a-9c0f-1a2b3c4d5e6f"
API <- "https://v2.plasticdb.org/api/v1"

read_fixture <- function(name) {
    jsonlite::read_json(file.path(src, paste0(name, ".json")),
        simplifyVector = FALSE)
}

to_json <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null",
        digits = NA))
}

with_id <- function(body, id) {
    text <- gsub(PY_JOB_ID, id, to_json(body), fixed = TRUE)
    jsonlite::parse_json(text, simplifyVector = FALSE)
}

# The file httptest2 looks for (helper-mock.R maps /api/v1/jobs etc. to
# synthetic/).
mock_file <- function(method, url) {
    short <- sub(paste0(API, "/"), "synthetic/", url, fixed = TRUE)
    req <- httr2::req_method(httr2::request(short), method)
    file.path("tests/testthat/fixtures", paste0(httptest2::build_mock_url(req),
        ".R"))
}

write_response <- function(method, url, status, headers, body) {
    path <- mock_file(method, url)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (is.list(body)) body <- to_json(body)
    header_text <- paste(sprintf("`%s` = %s", names(headers),
        vapply(headers, function(h) deparse(h), "")), collapse = ", ")
    text <- sprintf(paste0("structure(list(method = \"%s\", url = \"%s\", ",
        "status_code = %dL, headers = structure(list(%s), class = ",
        "\"httr2_headers\"), body = charToRaw(%s), cache = new.env(parent = ",
        "emptyenv())), class = \"httr2_response\")"), method,
        sub(paste0(API, "/"), "pdb/", url, fixed = TRUE), as.integer(status),
        header_text, paste(deparse(body, width.cutoff = 500L),
            collapse = "\n"))
    writeLines(text, path)
    message("wrote ", path)
}

fixture <- function(name, id, method, url, status = NULL) {
    doc <- read_fixture(name)
    write_response(method, url, status %||% doc$status,
        doc$headers, with_id(doc$body, id))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

read_url <- function(id) sprintf("%s/jobs/%s?include=none", API, id)
results_url <- function(id) sprintf("%s/jobs/%s/results?limit=500&offset=0",
    API, id)

# Submissions (POST: helper-mock.R serves these whatever the body).
fixture("job_submitted", "done1", "POST", paste0(API, "/jobs"))
fixture("tree_done", "tree1", "POST", paste0(API, "/tree/build"), 201L)

# Job reads (?include=none) and their result pages.
fixture("job_done", "done1", "GET", read_url("done1"))
fixture("job_done_results", "done1", "GET", results_url("done1"))
fixture("job_running", "run1", "GET", read_url("run1"))
fixture("job_error", "err1", "GET", read_url("err1"))
fixture("job_compare", "cmp1", "GET", read_url("cmp1"))
fixture("job_compare_results", "cmp1", "GET", results_url("cmp1"))
fixture("job_hmm", "hmm1", "GET", read_url("hmm1"))
fixture("job_hmm_results", "hmm1", "GET", results_url("hmm1"))
fixture("job_pathway", "pwy1", "GET", read_url("pwy1"))
fixture("job_annotate_list", "taxa1", "GET", read_url("taxa1"))
fixture("tree_detail", "tree1", "GET", read_url("tree1"))
fixture("jobs_list", "done1", "GET", paste0(API, "/jobs?limit=500&offset=0"))

# A compare_genomes job's matrix (GET /jobs/{id}/comparison?format=json).
cmp <- read_fixture("job_compare")
write_response("GET", sprintf("%s/jobs/cmp1/comparison?format=json", API),
    200L, cmp$headers, list(comparison = cmp$body$comparison,
        view = list(min_scov = 70, min_qcov = NULL, min_bitscore = NULL,
            all = FALSE)))

# Result files: the TSVs carry `#` provenance lines.
tsv_headers <- function(name) list(`content-type` =
    "text/tab-separated-values; charset=utf-8",
    `content-disposition` = sprintf("attachment; filename=\"%s\"", name),
    `x-plasticdb-release` = "2026.30",
    `x-plasticdb-release-source` = "snapshot")
write_response("GET", sprintf("%s/jobs/done1/download", API), 200L,
    tsv_headers("plasticdb_annotate_genome_done1.tsv"), paste0(
        "# PlasticDB annotate_genome job done1\n",
        "# release: 2026.30 (snapshot)\n",
        "query_seq_id\tsubject_accession\tpident\tscov\n",
        "contig_1_0\tPBFCTW\t91.5\t99.3\n"))
write_response("GET", sprintf("%s/jobs/taxa1/download", API), 200L,
    tsv_headers("plasticdb_annotate_list_taxa1.tsv"), paste0(
        "# PlasticDB annotate_list job taxa1\n",
        "# release: 2026.30 (snapshot)\n",
        "Sample\tGenus\tSpecies\tPlasticDB_match\tPlasticDB_plastics\n",
        "s1\tIdeonella\tsakaiensis\tIdeonella sakaiensis\tPET\n",
        "s2\tEscherichia\tcoli\t\t\n"))

# Share links.
write_response("GET", sprintf("%s/jobs/done1/shares", API), 200L,
    list(`content-type` = "application/json"), list(
        list(id = 7L, created_at = "2026-11-18T02:00:00Z", active = TRUE,
            expires_at = "2026-12-18T02:00:00Z", revoked_at = NULL),
        list(id = 6L, created_at = "2026-11-18T01:30:00Z", active = FALSE,
            expires_at = NULL, revoked_at = "2026-11-18T01:45:00Z")))
write_response("POST", sprintf("%s/jobs/done1/shares", API), 201L,
    list(`content-type` = "application/json"), list(id = 7L,
        created_at = "2026-11-18T02:00:00Z", active = TRUE,
        expires_at = "2026-12-18T02:00:00Z", revoked_at = NULL,
        token = "tok123", path = "/jobs/shared/tok123"))
shared <- read_fixture("job_done")
write_response("GET", sprintf("%s/jobs/shared/tok123?include=none", API),
    200L, shared$headers, with_id(shared$body, "done1"))
shared_hits <- read_fixture("job_done_results")
write_response("GET", sprintf(
    "%s/jobs/shared/tok123/results?limit=500&offset=0", API), 200L,
    shared_hits$headers, with_id(shared_hits$body, "done1"))

# A synchronous sequence search (POST, served whatever the query).
write_response("POST", sprintf("%s/bioinformatics/search-sequence", API),
    200L, list(`content-type` = "application/json"), list(
        hits = list(list(query_seq_id = "query",
            subject_seq_id = "protein|PBFCTW", pident = 100, length = 290L,
            mismatch = 0L, gapopen = 0L, qstart = 1L, qend = 290L,
            sstart = 1L, send = 290L, evalue = 1e-180, bitscore = 590.1,
            plasticdb_id = "PlasticDB_17", accession = "PBFCTW",
            protein_name = "PETase", entity_type = "protein",
            entity_id = 17L, detail_url = "/proteins/PBFCTW",
            variant_type = NULL)),
        release = NULL, release_source = "live",
        reference_sha256 = strrep("9d", 32), diamond_version = "2.1.9"))
