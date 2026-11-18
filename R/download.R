# Streaming downloads to disk, with checksum verification and the release cache.

filename_from <- function(headers, default) {
    cd <- headers[["content-disposition"]] %||% ""
    m <- regmatches(cd, regexec(
        "filename\\*?=(?:UTF-8'')?\"?([^\";]+)\"?", cd, ignore.case = TRUE,
        perl = TRUE))[[1]]
    name <- if (length(m) >= 2L) basename(m[[2]]) else ""
    if (nzchar(name) && !name %in% c(".", "..")) name else default
}

resolve_target <- function(dest, headers, default) {
    dest <- path.expand(dest)
    if (dir.exists(dest) || grepl("[/\\\\]$", dest)) {
        return(file.path(sub("[/\\\\]+$", "", dest),
            filename_from(headers, default)))
    }
    dest
}

place_file <- function(source, target) {
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(paste0(".", basename(target), "."), tmpdir = dirname(target))
    on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
    if (!file.copy(source, tmp, overwrite = TRUE)) {
        stop("Could not write ", target, call. = FALSE)
    }
    if (!file.rename(tmp, target)) {
        file.copy(tmp, target, overwrite = TRUE)
    }
    target
}

# Stream a file to `dest` (a file, or a directory to name it in) and return
# its path. Redirects are followed. With `sha256` the file is verified before
# it is moved into place. A pinned snapshot answer (or any answer for
# `immutable_release`, a release-scoped artefact) is kept in the cache.
pdb_download <- function(client, endpoint, dest, params = list(),
                         path_params = list(), sha256 = NULL,
                         immutable_release = NULL, default_name = "download") {
    client <- check_client(client)
    prep <- prepare_request(client, endpoint, params, path_params)
    expected <- if (is.null(sha256)) NULL else tolower(sha256)
    cache_release <- immutable_release %||% prep$pinned
    key <- if (!is.null(client$cache_dir) && !is.null(cache_release))
        cache_key_for(prep$method, prep$url)
    if (!is.null(key)) {
        hit <- cache_get_file(client, cache_release, key)
        if (!is.null(hit) && (is.null(expected) ||
            identical(hit$sha256, expected))) {
            target <- resolve_target(dest, hit$headers, default_name)
            place_file(hit$path, target)
            remember(client, decode_response(hit$status, hit$headers, raw(),
                prep$url, from_cache = TRUE), endpoint)
            return(target)
        }
    }

    req <- build_httr2(client, prep$method, prep$url, follow_redirects = TRUE)
    tmp <- tempfile("plasticdb-")
    on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
    resp <- send_request(client, req, path = tmp)
    status <- httr2::resp_status(resp)
    headers <- response_headers(resp)
    if (status < 200L || status >= 300L) {
        abort_from_response(decode_response(status, headers,
            response_bytes(resp), prep$url))
    }
    body_to_file(resp, tmp)
    actual <- sha256_file(tmp)
    if (!is.null(expected) && !identical(actual, expected)) {
        pdb_abort("checksum", sprintf(
            "%s: SHA-256 %s does not match the published %s",
            sub("\\?.*$", "", prep$url), actual, expected),
            expected = expected, actual = actual, url = prep$url,
            status = status)
    }
    target <- resolve_target(dest, headers, default_name)
    place_file(tmp, target)
    result <- decode_response(status, headers, raw(), prep$url)
    remember(client, result, endpoint)
    if (!is.null(key)) {
        storable <- !is.null(immutable_release) ||
            (identical(result$release_source, "snapshot") &&
                identical(result$release, prep$pinned))
        if (storable) {
            cache_put_file(client, cache_release, key, target, status,
                headers, prep$url, actual)
        }
    }
    target
}
