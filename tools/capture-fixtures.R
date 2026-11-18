# Re-capture the httptest2 fixtures in tests/testthat/fixtures from the live,
# anonymous API. Run from the package root:  Rscript tools/capture-fixtures.R
#
# Responses are saved whole (status, headers, body) so the tests see the
# release headers. Long nested arrays are trimmed to a few elements to keep
# the fixtures small; URLs are shortened ("pdb/" for the API root) to keep the
# paths under R's 100-byte tarball limit. tests/testthat/helper-mock.R applies
# the same shortening to requests.
#
# The job fixtures (fixtures/synthetic) are not captured: jobs need an API key.
# tools/make-job-fixtures.R writes them; this script leaves them alone.

pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
library(httptest2)

dir <- "tests/testthat/fixtures"
unlink(file.path(dir, c("pdb", "pdbroot")), recursive = TRUE)
.mockPaths(dir)
options(plasticdb.quiet = TRUE, plasticdb.cache = FALSE)

shorten <- function(url) {
    url <- sub("https://v2.plasticdb.org/api/v1/", "pdb/", url, fixed = TRUE)
    sub("https://v2.plasticdb.org/", "pdbroot/", url, fixed = TRUE)
}

trim <- function(x, depth = 0L, keep = 3L) {
    if (is.list(x) && is.null(names(x))) {
        if (length(x) > keep) x <- x[seq_len(keep)]
        return(lapply(x, trim, depth = depth + 1L, keep = keep))
    }
    if (is.list(x)) {
        return(lapply(x, trim, depth = depth + 1L, keep = keep))
    }
    x
}

set_redactor(function(resp) {
    resp$url <- shorten(resp$url)
    resp$request$url <- shorten(resp$request$url)
    type <- resp$headers[["content-type"]] %||% ""
    resp$headers <- resp$headers[setdiff(tolower(names(resp$headers)),
        c("set-cookie", "date", "server", "x-ratelimit-reset",
          "strict-transport-security"))]
    if (grepl("json", type) && length(resp$body)) {
        data <- jsonlite::parse_json(rawToChar(resp$body),
            simplifyVector = FALSE)
        data <- trim(data)
        resp$body <- charToRaw(as.character(jsonlite::toJSON(data,
            auto_unbox = TRUE, null = "null", digits = NA)))
    }
    resp
})

`%||%` <- function(x, y) if (is.null(x)) y else x
try_call <- function(expr) tryCatch(expr, error = function(e) {
    message("expected or not: ", conditionMessage(e))
})

start_capturing(simplify = FALSE)
try_call(server_version())
try_call(releases_current())
try_call(releases_list())
try_call(releases_status())
try_call(releases_get("2026.99"))

try_call(proteins_list(plastic = "PET", kind = "wild_type", limit = 3))
try_call(proteins_list(sort_by = "nope", limit = 1))
try_call(proteins_get("PBFCTW"))
try_call(proteins_get("PBGQPC"))
try_call(proteins_get("PBMW9H"))
try_call(proteins_get("PBDTZX"))
try_call(proteins_interactions("PBFCTW", limit = 3))
try_call(proteins_interactions("PBDTZX", limit = 3))
try_call(proteins_kinetics("PBFCTW", limit = 2))
try_call(proteins_kinetics("PBGQPC", limit = 2))
try_call(proteins_properties("PBFCTW", limit = 2))
try_call(proteins_variants("PBFCTW", limit = 2))
try_call(proteins_sequences(plastic = "PHB", kind = "wild_type",
    min_year = 2025))
try_call(proteins_fasta("PBFCTW"))
try_call(proteins_engineered_parents(limit = 2))

try_call(variants_list(plastic = "PET", limit = 2))
try_call(variants_get("PBG3V3"))
try_call(variants_get("PBTNBA"))
try_call(variants_get("PB8B8P"))
try_call(variants_fasta("PBDTZX"))

try_call(microorganisms_list(genus = "Pseudomonas", limit = 2))
try_call(microorganisms_get(1547922))
try_call(microorganisms_names())
try_call(microorganisms_lineage_levels())
try_call(microorganisms_taxonomy("genus", "Ideonella"))

try_call(plastics_list(limit = 2))
try_call(plastics_get(16, microorganism_limit = 2, protein_limit = 2))
try_call(plastics_names())

try_call(papers_list(limit = 2))
try_call(papers_get(1262))
try_call(papers_footprint(1262))
try_call(papers_export(1262))

try_call(interactions_list(plastic = "PET", protein = "PBFCTW", limit = 2))
try_call(experiments_list(limit = 2))
try_call(observations_list(limit = 2))
try_call(interactions_get(1))
try_call(experiments_get(1))
try_call(observations_get(1))
try_call(evidence_binding_assays(limit = 2))
try_call(evidence_constructs(protein = "PBFCTW", limit = 2))
try_call(evidence_construct(397))
try_call(evidence_construct_structures(397))
try_call(evidence_preparations(limit = 2))
try_call(evidence_preparation(127))

try_call(search_hits("cutinase", limit = 3))
try_call(search_grouped("cutinase", limit = 1))
try_call(search_autocomplete("cutin"))

try_call(pathways_list())
try_call(pathways_get(1))
try_call(pathways_for_plastic(16))
try_call(pathways_graph(16))

try_call(stats_summary())
try_call(stats_protein_activity())
try_call(stats_charts())
try_call(stats_by_country())

try_call(changes(since = "2026-11-15T00:00:00Z", limit = 3))
try_call(downloads_plastics(tempdir(), format = "csv"))
stop_capturing()

# httptest2 sources fixture files without attaching itself.
for (f in list.files(file.path(dir, c("pdb", "pdbroot")), "\\.R$",
    recursive = TRUE, full.names = TRUE)) {
    text <- readLines(f, warn = FALSE)
    text <- gsub("(?<!::)find_mock_file\\(", "httptest2::find_mock_file(", text,
        perl = TRUE)
    writeLines(text, f)
}
