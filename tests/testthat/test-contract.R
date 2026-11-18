# Every operation the package calls must exist in the vendored SDK contract
# (tests/testthat/contract/sdk-v1.json, the server's /api/v1/openapi.sdk.json).

spec <- jsonlite::read_json(test_path("contract", "sdk-v1.json"))
prefix <- "/api/v1"
normalise <- function(path) gsub("\\{[^}]+\\}", "{}", path)
endpoints <- plasticdb:::ENDPOINTS
in_contract <- Filter(function(e) e$in_contract, endpoints)

operations <- unlist(lapply(names(spec$paths), function(path) {
    methods <- intersect(names(spec$paths[[path]]),
        c("get", "post", "put", "patch", "delete", "head"))
    paste(toupper(methods), normalise(path))
}))
op_key <- function(e) paste(e$method, normalise(paste0(prefix, e$path)))
operation <- function(e) {
    target <- normalise(paste0(prefix, e$path))
    path <- names(spec$paths)[normalise(names(spec$paths)) == target][[1]]
    spec$paths[[path]][[tolower(e$method)]]
}
param_names <- function(e) {
    vapply(operation(e)$parameters %||% list(), `[[`, "", "name")
}
`%||%` <- function(x, y) if (is.null(x)) y else x

test_that("the vendored document is the SDK contract", {
    expect_match(spec$openapi, "^3\\.")
    expect_true("PlasticDBKey" %in% names(spec$components$securitySchemes))
})

test_that("every endpoint the package calls is in the contract", {
    for (e in in_contract) {
        expect_true(op_key(e) %in% operations, info = op_key(e))
    }
})

test_that("pinnable reads accept ?release=, and unpinnable ones do not", {
    for (e in in_contract) {
        if (e$method != "GET") next
        expect_identical("release" %in% param_names(e), e$pinnable,
            info = op_key(e))
    }
})

test_that("live_only matches the 409 the server documents", {
    for (e in in_contract) {
        if (e$method != "GET") next
        expect_identical("409" %in% names(operation(e)$responses),
            e$live_only, info = op_key(e))
    }
})

# Contract operations deliberately not wrapped (as in the Python client):
# site plumbing, or duplicates of a wrapped call.
not_wrapped <- c(
    "GET /api/v1/auth/me",
    "GET /api/v1/compounds",
    "GET /api/v1/compounds/{}",
    "GET /api/v1/microorganisms/accession/{}",
    "GET /api/v1/microorganisms/search",
    "GET /api/v1/proteins/accession/{}",
    "GET /api/v1/snapshots",
    "GET /api/v1/snapshots/latest",
    "GET /api/v1/snapshots/{}",
    "GET /api/v1/vocabularies",
    "GET /api/v1/vocabulary",
    "GET /api/v1/vocabulary/{}"
)
is_job <- function(op) {
    grepl("^[A-Z]+ /api/v1/(jobs|tree|bioinformatics)", op)
}

test_that("every non-job contract operation is wrapped or listed", {
    wrapped <- vapply(in_contract, op_key, "")
    missing <- setdiff(operations, wrapped)
    # Jobs (submit, wait, results, share) are the next phase (R3).
    expect_setequal(missing[!is_job(missing)], not_wrapped)
    expect_length(intersect(not_wrapped, wrapped), 0)
    expect_false(any(is_job(wrapped)))
})

test_that("only /health is outside the contract", {
    outside <- Filter(function(e) !e$in_contract, endpoints)
    expect_identical(vapply(outside, `[[`, "", "path"), c(health = "/health"))
})
