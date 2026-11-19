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
#' server's message), `reason` (the server's machine-readable reason, when it
#' sent one: `withdrawn`, `expired`, `archive_unavailable`, `timeout`, ...),
#' `response_body` and `url`. The classes, most general first (extra fields
#' in brackets):
#'
#' * `plasticdb_error`: any of the below.
#' * `plasticdb_connection_error`: the server could not be reached;
#'   `plasticdb_timeout`: a request timed out (also a connection error).
#' * `plasticdb_unexpected_response`: an answer this package does not
#'   understand.
#' * `plasticdb_bad_request` (400), `plasticdb_authentication` (401: no,
#'   invalid or expired API key), `plasticdb_permission_denied` (403),
#'   `plasticdb_not_found` (404), `plasticdb_conflict` (409),
#'   `plasticdb_gone` (410: a withdrawn record, with `reason` `"withdrawn"`
#'   and its `accession`; a variant is withdrawn with its parent protein),
#'   `plasticdb_payload_too_large` (413: a file over the size limit, or more
#'   than 20 genomes).
#' * `plasticdb_api_key_required`: a job function was called without an API
#'   key; also `plasticdb_authentication`, signalled before anything is sent.
#' * `plasticdb_merged`: the accession was merged (the server answers 301);
#'   also `plasticdb_gone` (`merged_into`, `location`, and `kind`, what the
#'   survivor is: `"wild_type"`, `"engineered"` or `"microorganism"`).
#' * `plasticdb_engineered_variant`: a per-protein call given a variant's
#'   accession; also `plasticdb_not_found` (`accession`, `location`, and
#'   `merged_into` when the variant was merged too).
#' * `plasticdb_results_expired`: a job's result files were deleted under the
#'   retention policy (410 `expired`); also `plasticdb_gone`
#'   (`results_expired_at`). Its record and hits stay readable.
#' * `plasticdb_validation` (422): a parameter was invalid (`errors`, each
#'   with its `loc`, `msg` and `type`).
#' * `plasticdb_idempotency_key_reused`: a submission's `idempotency_key`
#'   was used before for a different request; also `plasticdb_validation`.
#' * `plasticdb_rate_limit` (429) beyond the retry budget (`retry_after`).
#' * `plasticdb_server_error` (5xx; a sequence search past the server's 15
#'   seconds is a 504 with `reason` `"timeout"`);
#'   `plasticdb_service_unavailable` (503, also a server error;
#'   `retry_after`, and `reason`: `archive_unavailable` while the dataset ZIP
#'   is built, `index_unavailable` or `binary_unavailable` for the sequence
#'   search).
#' * `plasticdb_job_error`: a problem with a job (`job`);
#'   `plasticdb_job_failed`: it finished with status `error` (the message is
#'   the server's reason); `plasticdb_job_timeout`: [wait_job()] ran out of
#'   time (the job keeps running on the server). Both are job errors.
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
    engineered_variant = c("plasticdb_engineered_variant",
        "plasticdb_not_found"),
    results_expired = c("plasticdb_results_expired", "plasticdb_gone"),
    idempotency_key_reused = c("plasticdb_idempotency_key_reused",
        "plasticdb_validation"),
    api_key_required = c("plasticdb_api_key_required",
        "plasticdb_authentication"),
    job_error = "plasticdb_job_error",
    job_failed = c("plasticdb_job_failed", "plasticdb_job_error"),
    job_timeout = c("plasticdb_job_timeout", "plasticdb_job_error"),
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
    named <- is.list(body) && !is.null(names(body))
    detail <- if (named) body$detail else body
    message <- error_message(status, detail)
    reason <- if (named) body_text_field(body, "reason")
    common <- list(status = status, detail = detail, response_body = body,
        url = resp$url, reason = reason)
    abort_with <- function(.kind, msg, ...) {
        do.call(pdb_abort, c(list(.kind, msg), common, list(...),
            list(call = call)))
    }

    if (status %in% c(301L, 308L) && named && is.character(body$accession)) {
        abort_redirect(resp, body, abort_with)
    }
    abort_special(status, reason, body, detail, message, abort_with)
    abort_release_reason(resp, status, reason, body, message, abort_with)
    if (status == 422L && length(unsupported_parameters(detail))) {
        common$reason <- "release_unsupported"
        abort_with("release_unsupported_filter", message,
            parameters = unsupported_parameters(detail),
            errors = validation_errors(detail))
    }
    abort_by_status(resp, status, body, named, detail, message, abort_with)
}

# Answers recognised by status and reason: engineered variant, expired
# results, a reused idempotency key. Returns when none applies.
abort_special <- function(status, reason, body, detail, message, abort_with) {
    if (status == 404L && identical(reason, "engineered_variant")) {
        accession <- body_text_field(body, "accession") %||% ""
        abort_with("engineered_variant", paste0(variant_message(accession),
            " (variants have no per-protein sub-resources)."),
            accession = accession,
            location = body_text_field(body, "location"),
            merged_into = body_text_field(body, "merged_into"))
    }
    if (status == 410L && identical(reason, "expired")) {
        abort_with("results_expired", message,
            results_expired_at = parse_time(body_text_field(body,
                "results_expired_at")))
    }
    if (status == 422L && "idempotency_key_reused" %in% error_types(detail)) {
        abort_with("idempotency_key_reused", message,
            errors = validation_errors(detail))
    }
    invisible(NULL)
}

# A release problem named by the server's reason. Returns when none applies.
abort_release_reason <- function(resp, status, reason, body, message,
                                    abort_with) {
    if (is.null(reason)) {
        return(invisible(NULL))
    }
    kind <- switch(paste(status, reason),
        `404 unknown` = "release_unknown",
        `404 no_artefacts` = "release_no_artefacts",
        `409 live_only` = "release_live_only",
        `503 not_mounted` = "release_not_mounted",
        NULL)
    if (!is.null(kind)) {
        abort_with(kind, message, release = body_text_field(body,
            "release"), current_release = body_text_field(body,
            "current_release"),
            retry_after = if (status == 503L)
                parse_retry_after(resp$headers[["retry-after"]]))
    }
    invisible(NULL)
}

# The general mapping from the HTTP status; always signals.
abort_by_status <- function(resp, status, body, named, detail, message,
                               abort_with) {
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
        if (kind == "gone") {
            abort_with(kind, message, accession = if (named)
                body_text_field(body, "accession"))
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

# A record redirect: the accession was merged, or it names a variant.
abort_redirect <- function(resp, body, abort_with) {
    target <- body$accession
    location <- body_text_field(body, "location") %||%
        resp$headers[["location"]]
    if (identical(body$reason, "engineered_variant")) {
        abort_with("engineered_variant", variant_message(target),
            accession = target, location = location, merged_into = NULL)
    }
    merged_into <- body_text_field(body, "merged_into") %||% target
    abort_with("merged",
        sprintf("This accession was merged into %s.", merged_into),
        merged_into = merged_into, location = location,
        kind = body_text_field(body, "kind"))
}

variant_message <- function(accession) {
    sprintf("%s is an engineered variant: use variants_get(\"%s\")",
        accession, accession)
}

# A non-empty string field of a JSON object, or NULL.
body_text_field <- function(body, key) {
    value <- if (is.list(body)) body[[key]]
    if (is.character(value) && length(value) == 1L && !is.na(value) &&
        nzchar(value)) value else NULL
}

error_types <- function(detail) {
    unlist(lapply(validation_errors(detail), function(e)
        if (is.character(e$type)) e$type))
}

parse_time <- function(value) {
    if (is.null(value)) {
        return(NULL)
    }
    value <- sub("Z$", "+0000", sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2",
        value))
    value <- sub("\\.[0-9]+", "", value)
    out <- as.POSIXct(strptime(value, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
    if (is.na(out)) NULL else out
}
