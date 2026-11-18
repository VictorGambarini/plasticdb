# Fixtures were captured by tools/capture-fixtures.R with shortened URLs;
# map each request the same way so httptest2 finds its file.
# Jobs need an API key, so their fixtures are synthetic
# (tools/make-job-fixtures.R) and live apart from the captured ones.
shorten_url <- function(url) {
    url <- sub("^https://v2.plasticdb.org/api/v1/(jobs|tree|bioinformatics)",
        "synthetic/\\1", url)
    url <- sub("https://v2.plasticdb.org/api/v1/", "pdb/", url, fixed = TRUE)
    sub("https://v2.plasticdb.org/", "pdbroot/", url, fixed = TRUE)
}

# httptest2 applies the redactor to each request to find its mock file.
httptest2::set_redactor(function(x) {
    x$url <- shorten_url(x$url)
    x
})
httptest2::.mockPaths(testthat::test_path("fixtures"))

# Every test starts from a clean configuration: no pinned release, no key,
# no cache, no unpinned notice.
local_clean_env <- function(env = parent.frame()) {
    withr::local_envvar(PLASTICDB_API_KEY = NA, PLASTICDB_RELEASE = NA,
        PLASTICDB_BASE_URL = NA, PLASTICDB_CACHE_DIR = NA, .local_envir = env)
    withr::local_options(plasticdb.release = NULL, plasticdb.base_url = NULL,
        plasticdb.cache = FALSE, plasticdb.quiet = TRUE,
        .local_envir = env)
}

# A synthetic JSON response for httr2::local_mocked_responses().
json_response <- function(body, status = 200L, release = NULL,
                          source = "live", headers = list(),
                          url = "https://v2.plasticdb.org/api/v1/x") {
    h <- c(list(`content-type` = "application/json",
        `x-plasticdb-release-source` = source),
        if (!is.null(release)) list(`x-plasticdb-release` = release),
        headers)
    httr2::response(status_code = status, url = url, headers = h,
        body = charToRaw(as.character(jsonlite::toJSON(body,
            auto_unbox = TRUE, null = "null"))))
}

page <- function(items, total = length(items)) {
    list(total = total, items = items)
}

# Mock every request with `fun(req)`; returns an environment whose `urls`
# records each requested URL.
local_api <- function(fun, env = parent.frame()) {
    calls <- new.env()
    calls$urls <- character()
    httr2::local_mocked_responses(function(req) {
        calls$urls <- c(calls$urls, req$url)
        fun(req)
    }, env = env)
    calls
}

local_no_sleep <- function(env = parent.frame()) {
    sleeps <- new.env()
    sleeps$waits <- numeric()
    testthat::local_mocked_bindings(pdb_sleep = function(seconds) {
        sleeps$waits <- c(sleeps$waits, seconds)
    }, .package = "plasticdb", .env = env)
    sleeps
}

query_of <- function(url) {
    q <- sub("^[^?]*\\??", "", url)
    if (!nzchar(q)) return(list())
    pairs <- strsplit(strsplit(q, "&", fixed = TRUE)[[1]], "=", fixed = TRUE)
    stats::setNames(lapply(pairs, function(p) utils::URLdecode(p[2])),
        vapply(pairs, `[`, "", 1))
}

# Serve every request from the httptest2 fixtures, recording each request
# (method, url, headers, body). A POST or DELETE is looked up by method and
# path alone (its body and query are what the tests assert on); a DELETE with
# no fixture answers 204. `override(req)` may answer a request first.
local_job_api <- function(override = NULL, env = parent.frame()) {
    calls <- new.env()
    calls$requests <- list()
    httr2::local_mocked_responses(function(req) {
        calls$requests[[length(calls$requests) + 1L]] <- req
        if (!is.null(override)) {
            out <- override(req)
            if (!is.null(out)) return(out)
        }
        method <- req$method %||% if (is.null(req$body)) "GET" else "POST"
        url <- shorten_url(req$url)
        if (method != "GET") url <- sub("\\?.*$", "", url)
        lookup <- httr2::req_method(httr2::request(url), method)
        file <- httptest2::find_mock_file(httptest2::build_mock_url(lookup))
        if (is.null(file)) {
            if (method == "DELETE") {
                return(httr2::response(status_code = 204L, method = method,
                    url = req$url))
            }
            stop("No fixture for ", method, " ", url, call. = FALSE)
        }
        resp <- source(file)$value
        resp$url <- req$url
        resp
    }, env = env)
    calls
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# A client with an API key and no cache.
key_client <- function(...) {
    plasticdb_client(api_key = "pdb_test_key", cache = FALSE, ...)
}

# The multipart fields of a recorded request, as name -> values (files as
# list(name, type, path)).
form_fields <- function(req) {
    data <- req$body$data
    lapply(data, function(v) {
        if (inherits(v, "form_file")) {
            list(name = v$name, type = v$type, path = v$path)
        } else {
            as.character(v)
        }
    })
}
