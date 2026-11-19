# Streaming downloads to disk, with checksum verification and the release cache.
#
# A release artefact is checked against the SHA-256 its release published and
# the one the server sends (X-PlasticDB-SHA256, also its strong ETag); a cached
# copy is verified locally and needs no request. A pinned download (a past
# release's table) is cached with its weak ETag; reusing it costs one
# conditional request (If-None-Match: a 304 has no body), and if the server
# cannot be reached the cached copy is used.

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
    tmp <- tempfile(paste0(".", basename(target), "."),
        tmpdir = dirname(target))
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
                            immutable_release = NULL,
                                default_name = "download") {
    client <- check_client(client)
    prep <- prepare_request(client, endpoint, params, path_params)
    expected <- if (is.null(sha256)) NULL else tolower(sha256)
    cache_release <- immutable_release %||% prep$pinned
    key <- if (!is.null(client$cache_dir) && !is.null(cache_release))
        cache_key_for(prep$method, prep$url)
    hit <- usable_hit(client, cache_release, key, expected)
    use_hit <- function() {
        place_cached(client, endpoint, prep, hit, dest, default_name)
    }
    etag <- if (!is.null(hit)) clean(hit$headers[["etag"]])
    # A hit that is content-addressed (verified above), or has no validator,
    # stands as it is.
    if (!is.null(hit) && (!is.null(immutable_release) || is.null(etag))) {
        return(use_hit())
    }
    conditional <- if (!is.null(etag)) list(`If-None-Match` = etag) else list()
    req <- build_httr2(client, prep$method, prep$url, follow_redirects = TRUE,
        headers = conditional)
    tmp <- tempfile("plasticdb-")
    on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
    resp <- send_download(client, req, tmp, prep, hit)
    if (is.null(resp)) {
        return(use_hit())
    }
    status <- httr2::resp_status(resp)
    if (status == 304L && !is.null(hit)) {
        return(use_hit())
    }
    got <- receive_download(resp, status, tmp, dest, expected, prep$url,
        default_name)
    target <- got$target
    headers <- got$headers
    actual <- got$actual
    result <- decode_response(status, headers, raw(), prep$url)
    remember(client, result, endpoint)
    if (!is.null(key) && (!is.null(immutable_release) ||
        (identical(result$release_source, "snapshot") &&
            identical(result$release, prep$pinned)))) {
        cache_put_file(client, cache_release, key, target, status,
            headers, prep$url, actual)
    }
    target
}

# Check a successful answer, verify its checksums and move the file into
# place; any other status is signalled as an error.
receive_download <- function(resp, status, tmp, dest, expected, url,
                                default_name) {
    headers <- response_headers(resp)
    if (status < 200L || status >= 300L) {
        abort_from_response(decode_response(status, headers,
            response_bytes(resp), url))
    }
    body_to_file(resp, tmp)
    actual <- sha256_file(tmp)
    verify_download(actual, expected, headers, url, status)
    target <- resolve_target(dest, headers, default_name)
    place_file(tmp, target)
    list(target = target, headers = headers, actual = actual)
}

# The cached copy of a download, unless it fails the expected checksum.
usable_hit <- function(client, release, key, expected) {
    hit <- if (!is.null(key)) cache_get_file(client, release, key)
    if (!is.null(hit) && !is.null(expected) &&
        !identical(hit$sha256, expected)) {
        return(NULL)
    }
    hit
}

# Send the download; an unreachable server with a cached copy `hit` warns
# and returns NULL (the caller uses the copy), without one it signals.
send_download <- function(client, req, tmp, prep, hit) {
    tryCatch(send_request(client, req, path = tmp),
        plasticdb_connection_error = function(e) {
            if (is.null(hit)) rlang::cnd_signal(e)
            rlang::warn(sprintf(
                "PlasticDB: %s unreachable (%s); using the cached copy.",
                sub("\\?.*$", "", prep$url), conditionMessage(e)),
                class = "plasticdb_cache_fallback")
            NULL
        })
}

# The file's SHA-256 must match the published one and the server's header.
verify_download <- function(actual, expected, headers, url, status) {
    served <- clean(tolower(headers[[SHA256_HEADER]] %||% ""))
    for (check in list(list(expected, "the published"),
                        list(served, "the server's"))) {
        want <- check[[1]]
        if (!is.null(want) && !identical(actual, want)) {
            pdb_abort("checksum", sprintf(
                "%s: SHA-256 %s does not match %s %s",
                sub("\\?.*$", "", url), actual, check[[2]], want),
                expected = want, actual = actual, url = url,
                status = status)
        }
    }
    invisible(actual)
}

# A cached copy placed at `dest`.
place_cached <- function(client, endpoint, prep, hit, dest, default_name) {
    target <- resolve_target(dest, hit$headers, default_name)
    place_file(hit$path, target)
    remember(client, decode_response(hit$status, hit$headers, raw(),
        prep$url, from_cache = TRUE), endpoint)
    target
}
