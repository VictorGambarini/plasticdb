RELEASE_HEADER <- "x-plasticdb-release"
RELEASE_SOURCE_HEADER <- "x-plasticdb-release-source"
UNAVAILABLE_HEADER <- "x-plasticdb-snapshot-unavailable"
SHA256_HEADER <- "x-plasticdb-sha256"
SOURCE_SHA256_HEADER <- "x-plasticdb-source-sha256"
IDEMPOTENCY_HEADER <- "Idempotency-Key"
REPLAYED_HEADER <- "idempotent-replayed"
RETRY_STATUS <- c(500L, 502L, 503L, 504L)
IDEMPOTENT_METHODS <- c("GET", "HEAD", "PUT", "DELETE", "OPTIONS")

# Query parameters as strings: NULL/NA dropped, logicals lower-case, dates ISO.
query_value <- function(value) {
    if (inherits(value, "POSIXt")) {
        return(format(value, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }
    if (inherits(value, "Date")) {
        return(format(value, "%Y-%m-%dT00:00:00Z"))
    }
    if (is.logical(value)) {
        return(ifelse(value, "true", "false"))
    }
    if (is.numeric(value)) {
        return(format(value, scientific = FALSE, trim = TRUE,
            drop0trailing = TRUE, digits = 15))
    }
    as.character(value)
}

build_query <- function(params) {
    out <- list()
    for (name in names(params)) {
        value <- params[[name]]
        if (is.null(value)) next
        value <- value[!is.na(value)]
        if (!length(value)) next
        out[[name]] <- query_value(value)
    }
    out
}

# The URL (query sorted) and the release a request is pinned to.
prepare_request <- function(client, endpoint, params, path_params) {
    query <- build_query(params)
    if (endpoint$pinnable && !is.null(client$release) &&
        is.null(query$release)) {
        query$release <- client$release
    }
    base <- if (endpoint$root) client$root_url else client$base_url
    url <- paste0(base, ep_format(endpoint, path_params))
    if (length(query)) {
        query <- query[order(names(query))]
        pairs <- unlist(lapply(names(query), function(name) {
            paste0(curl_escape(name), "=", vapply(query[[name]], curl_escape,
                ""))
        }))
        url <- paste0(url, "?", paste(pairs, collapse = "&"))
    }
    pinned <- if (endpoint$pinnable && is.character(query$release))
        query$release[[1]] else NULL
    list(method = endpoint$method, url = url, pinned = pinned)
}

# `body` is list(json = <list>) or list(multipart = <list of fields>);
# `headers` a named list of extra request headers.
build_httr2 <- function(client, method, url, follow_redirects = FALSE,
                        headers = list(), body = NULL) {
    req <- httr2::request(url)
    req <- httr2::req_method(req, method)
    req <- httr2::req_user_agent(req, user_agent())
    req <- httr2::req_headers(req, Accept = "application/json")
    if (length(headers)) {
        req <- httr2::req_headers(req, !!!headers)
    }
    if (!is.null(client$api_key)) {
        req <- httr2::req_auth_bearer_token(req, client$api_key)
    }
    if (!is.null(body$json)) {
        req <- httr2::req_body_json(req, body$json, auto_unbox = TRUE,
            null = "null", digits = NA)
    } else if (!is.null(body$multipart)) {
        req <- httr2::req_body_multipart(req, !!!body$multipart)
    }
    req <- httr2::req_method(req, method)
    req <- httr2::req_options(req, followlocation = as.integer(
        follow_redirects))
    req <- httr2::req_timeout(req, client$timeout)
    httr2::req_error(req, is_error = function(resp) FALSE)
}

# Send with retries. 429 and transient 5xx wait for Retry-After (or back off)
# when that is within max_retry_wait; connection errors back off. A 503
# `not_mounted` (or a sequence search's 504 `timeout`) will not fix itself, so
# it is answered at once.
#
# A GET (or any `retry_safe` request: a search, or a submission whose
# Idempotency-Key makes a repeat return the first result) is resent on a
# connection error, a 5xx and a 429. Any other POST is resent only when it
# never reached the server (it could not connect) or on a 429, which the
# server refused unprocessed.
send_request <- function(client, req, path = NULL, retry_safe = NULL) {
    if (is.null(retry_safe)) {
        retry_safe <- toupper(req$method %||% "GET") %in% IDEMPOTENT_METHODS
    }
    attempt <- 0L
    repeat {
        resp <- tryCatch(httr2::req_perform(req, path = path),
            httr2_failure = function(e) e)
        if (inherits(resp, "httr2_failure")) {
            msg <- conditionMessage(resp)
            if (attempt < client$max_retries &&
                (retry_safe || never_sent(resp))) {
                pdb_sleep(backoff(client, attempt))
                attempt <- attempt + 1L
                next
            }
            if (grepl("timed? ?out|timeout", msg, ignore.case = TRUE)) {
                pdb_abort("timeout", paste("Request timed out:", msg),
                    parent = resp)
            }
            pdb_abort("connection_error",
                paste("Could not reach PlasticDB:", msg), parent = resp)
        }
        if (attempt >= client$max_retries) {
            return(resp)
        }
        status <- httr2::resp_status(resp)
        retryable <- retry_safe && status %in% RETRY_STATUS &&
            !is_permanent(resp)
        if (status != 429L && !retryable) {
            return(resp)
        }
        wait <- parse_retry_after(httr2::resp_header(resp, "retry-after"))
        if (is.null(wait)) wait <- backoff(client, attempt)
        if (wait > client$max_retry_wait) {
            return(resp)
        }
        pdb_sleep(wait)
        attempt <- attempt + 1L
    }
}

# A connection failure before anything was sent (the request cannot have
# reached the server).
never_sent <- function(failure) {
    msg <- paste(conditionMessage(failure),
        conditionMessage(failure$parent %||% failure))
    pattern <- paste("resolve host|resolve proxy|Failed to connect|",
        "connection refused|Couldn't connect|Could not connect", sep = "")
    grepl(pattern, msg, ignore.case = TRUE) &&
        !grepl("timed? ?out|timeout", msg, ignore.case = TRUE)
}

backoff <- function(client, attempt) {
    delay <- client$backoff_factor * 2^attempt
    min(delay + stats::runif(1, 0, delay / 4), client$max_retry_wait)
}

# Wrapped so tests can replace it with local_mocked_bindings().
pdb_sleep <- function(seconds) {
    Sys.sleep(seconds)
}

PERMANENT <- c("503 not_mounted", "504 timeout")

is_permanent <- function(resp) {
    status <- httr2::resp_status(resp)
    if (!status %in% c(503L, 504L)) {
        return(FALSE)
    }
    body <- tryCatch(parse_json_body(response_bytes(resp)),
        error = function(e) NULL)
    reason <- if (is.list(body)) body_text_field(body, "reason")
    !is.null(reason) && paste(status, reason) %in% PERMANENT
}

#' Seconds to wait from a Retry-After header (delta-seconds or HTTP-date)
#' @noRd
parse_retry_after <- function(value) {
    if (is.null(value) || length(value) != 1L || is.na(value) ||
        !nzchar(trimws(value))) {
        return(NULL)
    }
    value <- trimws(value)
    if (grepl("^-?[0-9]+(\\.[0-9]+)?$", value)) {
        return(max(as.numeric(value), 0))
    }
    lc <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", lc), add = TRUE)
    Sys.setlocale("LC_TIME", "C")
    when <- as.POSIXct(strptime(value, "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
    if (is.na(when)) {
        return(NULL)
    }
    max(as.numeric(difftime(when, Sys.time(), units = "secs")), 0)
}

response_bytes <- function(resp) {
    body <- resp$body
    if (inherits(body, "httr2_path")) {
        file <- unclass(body)
        if (!file.exists(file)) return(raw())
        return(readBin(file, "raw", file.size(file)))
    }
    if (is.raw(body)) body else raw()
}

# Make sure a response's body is in `file` (a mocked response keeps it in
# memory or in its own fixture file).
body_to_file <- function(resp, file) {
    body <- resp$body
    if (inherits(body, "httr2_path")) {
        src <- unclass(body)
        if (normalizePath(src, mustWork = FALSE) !=
            normalizePath(file, mustWork = FALSE)) {
            file.copy(src, file, overwrite = TRUE)
        }
    } else if (is.raw(body)) {
        writeBin(body, file)
    } else if (!file.exists(file)) {
        file.create(file)
    }
    file
}

parse_json_body <- function(bytes) {
    if (!length(bytes)) {
        return(NULL)
    }
    jsonlite::parse_json(rawToChar(bytes), simplifyVector = FALSE)
}

response_headers <- function(resp) {
    headers <- httr2::resp_headers(resp)
    out <- lapply(seq_along(headers), function(i) as.character(headers[[i]]))
    names(out) <- tolower(names(headers))
    out[!duplicated(names(out))]
}

# One decoded answer: status, data, headers and the release that produced it.
decode_response <- function(status, headers, bytes, url, from_cache = FALSE) {
    type <- headers[["content-type"]] %||% ""
    data <- if (status == 204L || !length(bytes)) {
        NULL
    } else if (grepl("json", type, fixed = TRUE)) {
        tryCatch(parse_json_body(bytes), error = function(e) {
            pdb_abort("unexpected_response", "The server sent invalid JSON",
                url = url, parent = e)
        })
    } else {
        bytes
    }
    list(status = status, data = data, headers = headers, url = url,
        release = clean(headers[[RELEASE_HEADER]]),
        release_source = clean(headers[[RELEASE_SOURCE_HEADER]]),
        unavailable_fields = header_list(headers[[UNAVAILABLE_HEADER]]),
        etag = clean(headers[["etag"]]),
        sha256 = clean(headers[[SHA256_HEADER]]),
        source_sha256 = header_list(headers[[SOURCE_SHA256_HEADER]]),
        replayed = identical(tolower(headers[[REPLAYED_HEADER]] %||% ""),
            "true"),
        from_cache = from_cache, retrieved_at = Sys.time())
}

header_list <- function(value) {
    parts <- trimws(strsplit(value %||% "", ",", fixed = TRUE)[[1]])
    parts[nzchar(parts)]
}

# Whether a request to `endpoint` may be resent after it reached the server:
# a GET, a search that creates nothing, or a create operation that honours
# Idempotency-Key when the client has a credential (the server keeps keys per
# user and ignores them on anonymous requests).
retry_safe_for <- function(client, endpoint) {
    endpoint$method %in% IDEMPOTENT_METHODS || endpoint$retry_safe ||
        (endpoint$idempotency && !is.null(client$api_key))
}

new_idempotency_key <- function() {
    bytes <- as.integer(openssl::rand_bytes(16))
    bytes[7] <- bitwOr(bitwAnd(bytes[7], 0x0f), 0x40)
    bytes[9] <- bitwOr(bitwAnd(bytes[9], 0x3f), 0x80)
    hex <- sprintf("%02x", bytes)
    paste(paste(hex[1:4], collapse = ""), paste(hex[5:6], collapse = ""),
        paste(hex[7:8], collapse = ""), paste(hex[9:10], collapse = ""),
        paste(hex[11:16], collapse = ""), sep = "-")
}

# Send one API call and decode it; signal a plasticdb_error on failure. On an
# `idempotency` endpoint every attempt carries the same Idempotency-Key.
pdb_request <- function(client, endpoint, params = list(),
                        path_params = list(), follow_redirects = FALSE,
                        record = TRUE, body = NULL, idempotency_key = NULL) {
    client <- check_client(client)
    prep <- prepare_request(client, endpoint, params, path_params)
    headers <- list()
    if (endpoint$idempotency) {
        headers[[IDEMPOTENCY_HEADER]] <- idempotency_key %||%
            new_idempotency_key()
    }
    cache_key <- NULL
    if (!is.null(client$cache_dir) && prep$method == "GET" &&
        !is.null(prep$pinned)) {
        cache_key <- cache_key_for(prep$method, prep$url)
        hit <- cache_get(client, prep$pinned, cache_key)
        if (!is.null(hit)) {
            result <- decode_response(hit$status, hit$headers, hit$body,
                prep$url, from_cache = TRUE)
            if (record) remember(client, result, endpoint)
            return(result)
        }
    }
    req <- build_httr2(client, prep$method, prep$url, follow_redirects,
        headers = headers, body = body)
    resp <- send_request(client, req,
        retry_safe = retry_safe_for(client, endpoint))
    status <- httr2::resp_status(resp)
    headers <- response_headers(resp)
    bytes <- response_bytes(resp)
    result <- decode_response(status, headers, bytes, prep$url)
    if (status < 200L || status >= 300L) {
        abort_from_response(result)
    }
    if (record) remember(client, result, endpoint)
    if (!is.null(cache_key) && identical(result$release_source, "snapshot") &&
        identical(result$release, prep$pinned)) {
        cache_put(client, prep$pinned, cache_key, status, headers, bytes,
            prep$url)
    }
    result
}

remember <- function(client, result, endpoint) {
    .state$last <- result
    if (endpoint$pinnable) {
        .state$last_read <- result
    }
    announce(client, result)
}

announce <- function(client, result) {
    if (.state$announced || !is.null(client$release) ||
        isTRUE(getOption("plasticdb.quiet"))) {
        return(invisible())
    }
    .state$announced <- TRUE
    if (!is.null(result$release)) {
        rlang::inform(c(
            sprintf(paste("PlasticDB: unpinned, answered by the live",
                "database (release %s)."), result$release),
            i = sprintf(paste0("Set options(plasticdb.release = \"%s\") or ",
                "plasticdb_client(release = \"%s\") to make results ",
                "reproducible."), result$release, result$release)),
            class = "plasticdb_unpinned")
    } else {
        rlang::inform(paste("PlasticDB: unpinned client; the server named no",
            "dataset release (live database)."), class = "plasticdb_unpinned")
    }
}

#' The most recent answer from the API
#'
#' The status, headers, URL and release of the last request this session
#' made, for checking what answered (`release`, `release_source`, which is
#' `"snapshot"` for a past release and `"live"` for the live database,
#' `unavailable_fields`, the fields a release does not carry, `etag`,
#' `sha256` (a release artefact's own SHA-256), `source_sha256` (the release
#' Parquet files a pinned download was rendered from) and `replayed` (a
#' submission's `Idempotency-Key` returned an existing job).
#'
#' @return A list, or `NULL` before the first request.
#' @seealso [provenance()]
#' @export
#' @examples
#' last_response()
last_response <- function() {
    if (is.null(.state$last)) {
        return(NULL)
    }
    out <- .state$last
    out$data <- NULL
    out
}
