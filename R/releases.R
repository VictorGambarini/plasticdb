#' Dataset releases
#'
#' Releases are the named, citable versions of the data (e.g. `"2026.50"`),
#' each with a DOI, a frozen copy of the data and the reference databases the
#' analyses searched. Release metadata describes every release, so these
#' calls are never pinned.
#'
#' * `releases_current()`: the release the server is serving now (`version`
#'   is `NULL` before the first one).
#' * `releases_get()`: one release with its artefacts. An unknown version
#'   signals `plasticdb_release_unknown` (also a `plasticdb_not_found`).
#' * `releases_list()`: every completed release, newest first.
#' * `releases_status()`: whether a release is being cut right now.
#'
#' @inheritParams proteins
#' @param version A release version.
#'
#' @return A `plasticdb_record` list, or (`releases_list()`) a tibble.
#' @name releases
#' @examples
#' \dontrun{
#' releases_current()$version
#' releases_list()
#' }
NULL

#' @rdname releases
#' @export
releases_current <- function(client = plasticdb_client()) {
    get_record(client, ENDPOINTS$releases_current)
}

#' @rdname releases
#' @export
releases_get <- function(version, client = plasticdb_client()) {
    tryCatch(
        get_record(client, ENDPOINTS$releases_get,
            path_params = list(version = version)),
        plasticdb_not_found = function(e) {
            if (inherits(e, "plasticdb_release_unknown")) rlang::cnd_signal(e)
            pdb_abort("release_unknown", conditionMessage(e),
                release = version, reason = "unknown", status = e$status,
                detail = e$detail, response_body = e$response_body,
                url = e$url)
        })
}

#' @rdname releases
#' @export
releases_list <- function(limit = Inf, client = plasticdb_client()) {
    list_tibble(client, ENDPOINTS$releases_list, limit = limit)
}

#' @rdname releases
#' @export
releases_status <- function(client = plasticdb_client()) {
    get_record(client, ENDPOINTS$releases_status)
}
