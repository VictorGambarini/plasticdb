#' Errors and warnings raised by plasticdb
#'
#' Every error this package raises from the API is a condition of class
#' `plasticdb_error`, signalled with [rlang::abort()], so it can be caught by
#' class with [tryCatch()] or [rlang::try_fetch()]. The classes mirror the
#' exceptions of the Python client, and a condition carries every class it
#' belongs to: an unknown release is both a `plasticdb_release_unknown` and a
#' `plasticdb_not_found`, and a merged record is a `plasticdb_merged` and a
#' `plasticdb_gone`.
#'
#' Every condition has the fields `status` (the HTTP status), `detail` (the
#' server's message), `response_body` and `url`. The classes, most general
#' first (extra fields in brackets):
#'
#' * `plasticdb_error`: any of the below.
#' * `plasticdb_connection_error`: the server could not be reached;
#'   `plasticdb_timeout`: a request timed out (also a connection error).
#' * `plasticdb_unexpected_response`: an answer this package does not
#'   understand.
#' * `plasticdb_bad_request` (400), `plasticdb_authentication` (401: no,
#'   invalid or expired API key), `plasticdb_permission_denied` (403),
#'   `plasticdb_not_found` (404), `plasticdb_conflict` (409),
#'   `plasticdb_gone` (410: the record was withdrawn),
#'   `plasticdb_payload_too_large` (413).
#' * `plasticdb_merged`: the accession was merged; also `plasticdb_gone`
#'   (`merged_into`, `location`, `kind`).
#' * `plasticdb_engineered_variant`: a per-protein call given a variant's
#'   accession (`accession`, `location`).
#' * `plasticdb_validation` (422): a parameter was invalid (`errors`).
#' * `plasticdb_rate_limit` (429) beyond the retry budget (`retry_after`).
#' * `plasticdb_server_error` (5xx); `plasticdb_service_unavailable` (503,
#'   also a server error; `retry_after`).
#' * `plasticdb_checksum`: a download did not match its published SHA-256
#'   (`expected`, `actual`).
#' * `plasticdb_release_error`: a problem with the requested release
#'   (`release`, `current_release`, `reason`). Its subclasses:
#'   `plasticdb_release_not_available`, the release cannot answer at all,
#'   with `plasticdb_release_unknown` (404 `unknown`, also not found),
#'   `plasticdb_release_no_artefacts` (404 `no_artefacts`, also not found)
#'   and `plasticdb_release_not_mounted` (503 `not_mounted`, also service
#'   unavailable); and `plasticdb_release_unsupported`, the release cannot
#'   answer this call, with `plasticdb_release_live_only` (409 `live_only`,
#'   also a conflict) and `plasticdb_release_unsupported_filter` (422
#'   `release_unsupported`, also validation; `parameters`).
#'
#' A function given an argument it does not take (an unknown filter, say)
#' signals `plasticdb_argument_error` before anything is sent.
#'
#' Warnings: `plasticdb_merged_warning` when `follow_merges = TRUE` returns a
#' merge survivor, and `plasticdb_release_changed` when the release changed
#' while a list was being paged.
#'
#' @return Not a function: this page documents the condition classes.
#' @name plasticdb-conditions
#' @aliases plasticdb_error
#' @examples
#' ## An unknown filter is refused before any request is sent:
#' res <- tryCatch(proteins_list(no_such_filter = 1),
#'     plasticdb_argument_error = function(e) conditionMessage(e))
#' cat(res)
#' res
NULL

# Class chains, most specific first; "plasticdb_error" is appended by pdb_abort.
CONDITION_CLASSES <- list(
    connection_error = "plasticdb_connection_error",
    timeout = c("plasticdb_timeout", "plasticdb_connection_error"),
    unexpected_response = "plasticdb_unexpected_response",
    bad_request = "plasticdb_bad_request",
    authentication = "plasticdb_authentication",
    permission_denied = "plasticdb_permission_denied",
    not_found = "plasticdb_not_found",
    gone = "plasticdb_gone",
    merged = c("plasticdb_merged", "plasticdb_gone"),
    engineered_variant = "plasticdb_engineered_variant",
    conflict = "plasticdb_conflict",
    payload_too_large = "plasticdb_payload_too_large",
    validation = "plasticdb_validation",
    rate_limit = "plasticdb_rate_limit",
    server_error = "plasticdb_server_error",
    service_unavailable = c("plasticdb_service_unavailable",
        "plasticdb_server_error"),
    checksum = "plasticdb_checksum",
    release_error = "plasticdb_release_error",
    release_not_available = c("plasticdb_release_not_available",
        "plasticdb_release_error"),
    release_unknown = c("plasticdb_release_unknown",
        "plasticdb_release_not_available", "plasticdb_release_error",
        "plasticdb_not_found"),
    release_no_artefacts = c("plasticdb_release_no_artefacts",
        "plasticdb_release_not_available", "plasticdb_release_error",
        "plasticdb_not_found"),
    release_not_mounted = c("plasticdb_release_not_mounted",
        "plasticdb_release_not_available", "plasticdb_release_error",
        "plasticdb_service_unavailable", "plasticdb_server_error"),
    release_unsupported = c("plasticdb_release_unsupported",
        "plasticdb_release_error"),
    release_live_only = c("plasticdb_release_live_only",
        "plasticdb_release_unsupported", "plasticdb_release_error",
        "plasticdb_conflict"),
    release_unsupported_filter = c("plasticdb_release_unsupported_filter",
        "plasticdb_release_unsupported", "plasticdb_release_error",
        "plasticdb_validation")
)

pdb_abort <- function(.kind, message, ..., call = NULL) {
    classes <- CONDITION_CLASSES[[.kind]]
    if (is.null(classes)) {
        classes <- character()
    }
    rlang::abort(message, class = c(classes, "plasticdb_error"), ...,
        call = call)
}

arg_error <- function(message, call = NULL) {
    rlang::abort(message, class = "plasticdb_argument_error", call = call)
}

HTTP_REASONS <- c(
    `301` = "Moved Permanently", `308` = "Permanent Redirect",
    `400` = "Bad Request", `401` = "Unauthorized", `403` = "Forbidden",
    `404` = "Not Found", `409` = "Conflict", `410` = "Gone",
    `413` = "Payload Too Large", `422` = "Unprocessable Entity",
    `429` = "Too Many Requests", `500` = "Internal Server Error",
    `502` = "Bad Gateway", `503` = "Service Unavailable",
    `504` = "Gateway Timeout"
)

error_message <- function(status, detail) {
    reason <- HTTP_REASONS[as.character(status)]
    prefix <- trimws(paste(status, if (is.na(reason)) "" else reason))
    if (is.character(detail) && length(detail) == 1L && nzchar(detail)) {
        return(paste0(prefix, ": ", detail))
    }
    if (is.list(detail) && length(detail)) {
        parts <- character()
        for (err in detail) {
            if (is.list(err)) {
                loc <- paste(unlist(err$loc), collapse = ".")
                msg <- if (is.null(err$msg)) "" else as.character(err$msg)
                parts <- c(parts, if (nzchar(loc)) paste0(loc, ": ", msg)
                    else msg)
            }
        }
        if (length(parts)) {
            return(paste0(prefix, ": ", paste(parts, collapse = "; ")))
        }
    }
    prefix
}

# The accession segment of a /proteins/{accession}[/...] request.
requested_protein <- function(url) {
    path <- sub("\\?.*$", "", sub("^[a-z]+://[^/]+", "", url))
    parts <- strsplit(path, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    i <- match("proteins", parts)
    if (is.na(i) || i >= length(parts)) "" else utils::URLdecode(parts[i + 1])
}

validation_errors <- function(detail) {
    if (!is.list(detail)) {
        return(list())
    }
    Filter(is.list, detail)
}

unsupported_parameters <- function(detail) {
    out <- character()
    for (err in validation_errors(detail)) {
        if (identical(err$type, "release_unsupported") && length(err$loc)) {
            out <- c(out, as.character(utils::tail(unlist(err$loc), 1)))
        }
    }
    out
}

# Map a non-success answer onto the condition hierarchy (and signal it).
abort_from_response <- function(resp, call = NULL) {
    status <- resp$status
    body <- resp$data
    detail <- if (is.list(body) && !is.null(names(body))) body$detail else body
    message <- error_message(status, detail)
    common <- list(status = status, detail = detail, response_body = body,
        url = resp$url)
    abort_with <- function(.kind, msg, ...) {
        do.call(pdb_abort, c(list(.kind, msg), common, list(...),
            list(call = call)))
    }

    if (status %in% c(301L, 308L) && is.list(body) &&
        is.character(body$accession)) {
        abort_merged(resp, body, abort_with)
    }

    reason <- if (is.list(body) && !is.null(names(body))) body$reason
    if (is.character(reason) && length(reason) == 1L) {
        kind <- switch(paste(status, reason),
            `404 unknown` = "release_unknown",
            `404 no_artefacts` = "release_no_artefacts",
            `409 live_only` = "release_live_only",
            `503 not_mounted` = "release_not_mounted",
            NULL)
        if (!is.null(kind)) {
            abort_with(kind, message, release = body$release,
                current_release = body$current_release, reason = reason,
                retry_after = if (status == 503L)
                    parse_retry_after(resp$headers[["retry-after"]]))
        }
    }

    if (status == 422L && length(unsupported_parameters(detail))) {
        abort_with("release_unsupported_filter", message,
            reason = "release_unsupported",
            parameters = unsupported_parameters(detail),
            errors = validation_errors(detail))
    }
    if (status == 429L || status == 503L) {
        abort_with(if (status == 429L) "rate_limit" else "service_unavailable",
            message, retry_after = parse_retry_after(
                resp$headers[["retry-after"]]))
    }
    kind <- switch(as.character(status),
        `400` = "bad_request", `401` = "authentication",
        `403` = "permission_denied", `404` = "not_found", `409` = "conflict",
        `410` = "gone", `413` = "payload_too_large", `422` = "validation",
        NULL)
    if (!is.null(kind)) {
        if (kind == "validation") {
            abort_with(kind, message, errors = validation_errors(detail))
        }
        abort_with(kind, message)
    }
    if (status >= 500L) {
        abort_with("server_error", message)
    }
    if (status >= 300L && status < 400L) {
        abort_with("unexpected_response", sprintf(
            "Unexpected redirect (%d) to %s", status,
            resp$headers[["location"]] %||% "?"))
    }
    abort_with("unexpected_response", message)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# A merge redirect: the accession was merged, or it names a variant.
abort_merged <- function(resp, body, abort_with) {
    target <- body$accession
    location <- body$location %||% resp$headers[["location"]]
    if (identical(body$kind, "engineered") &&
        toupper(requested_protein(resp$url)) == toupper(target)) {
        abort_with("engineered_variant", sprintf(
            "%s is an engineered variant: use variants_get(\"%s\").",
            target, target), accession = target, location = location)
    }
    abort_with("merged",
        sprintf("This accession was merged into %s.", target),
        merged_into = target, location = location, kind = body$kind)
}
