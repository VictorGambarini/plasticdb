DEFAULT_BASE_URL <- "https://v2.plasticdb.org/api/v1"

# Session state: what answered last, whether the unpinned notice was shown, the
# server version, and open BiocFileCache handles. Never written to disk.
.state <- new.env(parent = emptyenv())
.state$last <- NULL
.state$last_read <- NULL
.state$announced <- FALSE
.state$server_version <- list()

#' Connect to PlasticDB
#'
#' `plasticdb_client()` describes how to reach the API: which server, which
#' dataset release to pin every read to, the API key, and where pinned answers
#' are cached. Every function in the package takes `client =
#' plasticdb_client()` as its last argument, so in most scripts you never call
#' it yourself: set the options or environment variables once and every call
#' follows them.
#'
#' Each setting is resolved, first match wins, from the argument, then the R
#' option, then the environment variable, then the default:
#'
#' | Argument | Option | Environment variable | Default |
#' |---|---|---|---|
#' | `api_key` | | `PLASTICDB_API_KEY` | none (reads are anonymous) |
#' | `release` | `plasticdb.release` | `PLASTICDB_RELEASE` | none (live database) |
#' | `base_url` | `plasticdb.base_url` | `PLASTICDB_BASE_URL` | `https://v2.plasticdb.org/api/v1` |
#' | `cache` | `plasticdb.cache` | `PLASTICDB_CACHE_DIR` | `TRUE` |
#' | `timeout` | `plasticdb.timeout` | | 30 seconds |
#' | `max_retries` | `plasticdb.max_retries` | | 3 |
#' | `max_retry_wait` | `plasticdb.max_retry_wait` | | 60 seconds |
#'
#' A pinned client sends `?release=` on every read; the server answers from
#' that release's snapshot. Answers from a past release are immutable, so they
#' are kept in a [BiocFileCache::BiocFileCache()] and reused without a request.
#' Live answers (including the current release, which the live database
#' serves) are never cached. `cache = TRUE` uses
#' `tools::R_user_dir("plasticdb", "cache")`, a string names another
#' directory, and `FALSE` turns caching off.
#'
#' Rate-limited requests (429) are retried after the server's `Retry-After`
#' when that is within `max_retry_wait`; requests are also retried with
#' backoff on 5xx answers and connection errors.
#'
#' @param api_key An API key, needed only for jobs. Never stored in an option.
#' @param release A dataset release version (e.g. `"2026.50"`) to pin every
#'   read to, or `NULL` for the live database.
#' @param base_url The API root.
#' @param cache `TRUE`, `FALSE`, or a cache directory.
#' @param timeout Request timeout in seconds.
#' @param max_retries How many times a request is retried.
#' @param max_retry_wait The longest wait (seconds) honoured before a retry.
#'
#' @return A `plasticdb_client` object (a list) to pass as `client =`.
#' @seealso [provenance()], [cite()], [releases_list()]
#' @export
#' @examples
#' client <- plasticdb_client(release = "2026.50", cache = FALSE)
#' client
#'
#' ## Or pin every call in a script:
#' old <- options(plasticdb.release = "2026.50")
#' plasticdb_client()$release
#' options(old)
plasticdb_client <- function(api_key = NULL, release = NULL, base_url = NULL,
                             cache = NULL, timeout = NULL,
                             max_retries = NULL, max_retry_wait = NULL) {
    api_key <- clean(api_key) %||% clean(Sys.getenv("PLASTICDB_API_KEY"))
    release <- clean(release) %||% clean(getOption("plasticdb.release")) %||%
        clean(Sys.getenv("PLASTICDB_RELEASE"))
    base_url <- clean(base_url) %||% clean(getOption("plasticdb.base_url")) %||%
        clean(Sys.getenv("PLASTICDB_BASE_URL")) %||% DEFAULT_BASE_URL
    base_url <- sub("/+$", "", base_url)
    if (is.null(cache)) {
        cache <- getOption("plasticdb.cache") %||%
            clean(Sys.getenv("PLASTICDB_CACHE_DIR")) %||% TRUE
    }
    cache_dir <- if (isTRUE(cache)) {
        tools::R_user_dir("plasticdb", "cache")
    } else if (is.character(cache) && length(cache) == 1L && nzchar(cache)) {
        path.expand(cache)
    } else if (isFALSE(cache)) {
        NULL
    } else {
        arg_error("`cache` must be TRUE, FALSE or a directory.")
    }
    timeout <- timeout %||% getOption("plasticdb.timeout", 30)
    max_retries <- max_retries %||% getOption("plasticdb.max_retries", 3L)
    max_retry_wait <- max_retry_wait %||%
        getOption("plasticdb.max_retry_wait", 60)
    if (!is.numeric(max_retries) || length(max_retries) != 1L ||
        max_retries < 0) {
        arg_error("`max_retries` must be a number >= 0.")
    }
    structure(list(
        api_key = api_key,
        release = release,
        base_url = base_url,
        root_url = root_of(base_url),
        cache_dir = cache_dir,
        timeout = as.numeric(timeout),
        max_retries = as.integer(max_retries),
        max_retry_wait = as.numeric(max_retry_wait),
        backoff_factor = 0.5
    ), class = "plasticdb_client")
}

#' @export
print.plasticdb_client <- function(x, ...) {
    cat("<plasticdb_client>\n")
    cat("  base_url: ", x$base_url, "\n", sep = "")
    cat("  release:  ", x$release %||% "none (live database)", "\n", sep = "")
    cat("  api_key:  ", if (is.null(x$api_key)) "none" else "set", "\n",
        sep = "")
    cat("  cache:    ", x$cache_dir %||% "off", "\n", sep = "")
    invisible(x)
}

check_client <- function(client) {
    if (!inherits(client, "plasticdb_client")) {
        arg_error("`client` must be made by plasticdb_client().")
    }
    client
}

#' The package's user agent
#'
#' @return A string, `plasticdb-r/<version> (R/<version>; httr2/<version>)`.
#' @keywords internal
#' @noRd
user_agent <- function() {
    sprintf("plasticdb-r/%s (R/%s; httr2/%s)",
        utils::packageVersion("plasticdb"), getRversion(),
        utils::packageVersion("httr2"))
}

clean <- function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x)) {
        return(NULL)
    }
    x <- trimws(as.character(x))
    if (nzchar(x)) x else NULL
}

root_of <- function(base_url) {
    if (endsWith(base_url, "/api/v1")) {
        return(substr(base_url, 1L, nchar(base_url) - nchar("/api/v1")))
    }
    sub("^([a-z]+://[^/]+).*$", "\\1", base_url)
}
