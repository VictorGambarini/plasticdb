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
# Jobs: the deprecated base64 routes are not wrapped (POST /jobs takes the
# same jobs as multipart uploads), nor GET /tree/{job_id}, a duplicate of
# GET /jobs/{job_id} (which carries result_newick).
not_wrapped_jobs <- c(
    "GET /api/v1/tree/{}"
)
deprecated <- unlist(lapply(names(spec$paths), function(path) {
    ops <- spec$paths[[path]]
    hits <- Filter(function(m) isTRUE(ops[[m]]$deprecated), names(ops))
    if (length(hits)) paste(toupper(hits), normalise(path))
}))

test_that("every contract operation is wrapped or listed", {
    wrapped <- vapply(in_contract, op_key, "")
    missing <- setdiff(operations, wrapped)
    expect_setequal(missing, c(not_wrapped, not_wrapped_jobs, deprecated))
    expect_length(intersect(c(not_wrapped, not_wrapped_jobs, deprecated),
        wrapped), 0)
})

test_that("the unwrapped bioinformatics routes are exactly the deprecated", {
    wrapped <- vapply(in_contract, op_key, "")
    bio <- grep("/api/v1/bioinformatics/", operations, value = TRUE)
    expect_setequal(setdiff(bio, wrapped), deprecated)
    expect_true(all(grepl("/bioinformatics/", deprecated)))
})

test_that("idempotency matches the Idempotency-Key the contract documents", {
    for (e in in_contract) {
        expect_identical("Idempotency-Key" %in% param_names(e),
            e$idempotency, info = op_key(e))
    }
})

test_that("every wrapped POST is idempotent or retry-safe", {
    for (e in in_contract) {
        if (e$method != "POST") next
        expect_true(e$idempotency || e$retry_safe, info = op_key(e))
        expect_identical("release" %in% param_names(e), e$pinnable,
            info = op_key(e))
    }
})

schema <- function(name) spec$components$schemas[[name]]

test_that("the job parameters are the submission form's fields", {
    form <- names(schema("Body_submit_job_api_v1_jobs_post")$properties)
    params <- unique(unlist(plasticdb:::JOB_PARAMS))
    expect_setequal(setdiff(form, c("job_type", "file", "files")), params)
    expect_setequal(names(plasticdb:::JOB_PARAMS),
        unlist(schema("JobTypeEnum")$enum))
    expect_setequal(plasticdb:::TREE_FILTERS,
        names(schema("TreeBuildRequest")$properties))
})

test_that("the synthetic job fixtures carry what the schemas require", {
    files <- list.files(test_path("fixtures", "synthetic"), "\\.R$",
        recursive = TRUE, full.names = TRUE)
    expect_gt(length(files), 0)
    for (f in files) {
        resp <- source(f)$value
        if (!grepl("json", resp$headers[["content-type"]] %||% "")) next
        body <- jsonlite::parse_json(rawToChar(resp$body))
        if (resp$status_code >= 300L) next
        if (all(c("total", "items") %in% names(body))) {
            items <- body$items
        } else if (is.null(names(body))) {
            items <- body
        } else {
            items <- list(body)
        }
        for (item in items) {
            name <- if (!is.null(item$job_type)) "JobRead" else
                if (!is.null(item$token)) "JobShareCreated" else
                if (!is.null(item$active)) "JobShareRead" else
                if (!is.null(item$query_seq_id)) "JobResultItem" else NULL
            if (is.null(name)) next
            required <- unlist(schema(name)$required)
            expect_true(all(required %in% names(item)),
                info = paste(basename(f), name))
            expect_true(all(names(item) %in% c(names(schema(name)$properties),
                if (name == "JobRead") names(schema("JobDetail")$properties))),
                info = paste(basename(f), name))
        }
    }
})

test_that("only /health is outside the contract", {
    outside <- Filter(function(e) !e$in_contract, endpoints)
    expect_identical(vapply(outside, `[[`, "", "path"), c(health = "/health"))
})

test_that("build_tree() is keyed: the contract has no anonymous option", {
    security <- operation(endpoints$tree_build)$security
    expect_gt(length(security), 0)
    expect_false(any(vapply(security, function(s) !length(s), TRUE)))
})

test_that("the evidence list filters are the contract's query parameters", {
    lists <- list(interactions = plasticdb:::INTERACTION_FILTERS,
        experiments = plasticdb:::EXPERIMENT_FILTERS,
        observations = plasticdb:::OBSERVATION_FILTERS)
    for (name in names(lists)) {
        e <- endpoints[[paste0(name, "_list")]]
        expect_setequal(setdiff(param_names(e), c("limit", "offset", "page",
            "release")), lists[[name]])
    }
    expect_true("include_sequence" %in% param_names(endpoints$proteins_list))
    expect_setequal(c(plasticdb:::PROTEIN_LIST_FILTERS,
        c("limit", "offset", "page", "release")),
        param_names(endpoints$proteins_list))
})

test_that("kind is the deprecated alias of evidence_level", {
    params <- operation(endpoints$proteins_interactions)$parameters
    kind <- Filter(function(p) identical(p$name, "kind"), params)[[1]]
    expect_true(isTRUE(kind$deprecated))
    expect_true("evidence_level" %in% param_names(
        endpoints$proteins_interactions))
})
