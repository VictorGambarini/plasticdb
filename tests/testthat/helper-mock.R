# Fixtures were captured by tools/capture-fixtures.R with shortened URLs;
# map each request the same way so httptest2 finds its file.
shorten_url <- function(url) {
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
