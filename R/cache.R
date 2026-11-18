# On-disk cache for answers from a past release's snapshot, in a BiocFileCache.
#
# A past release is immutable, so its answers are kept indefinitely. Only
# responses the server marks X-PlasticDB-Release-Source: snapshot (or files
# addressed by release version, which are immutable) are stored: live answers,
# including the current release, are never cached. Each entry is an .rds of
# {format, status, headers, size, sha256, body} named "<release>:<key>";
# downloaded files keep their body as a second resource "<release>:<key>:body".

CACHE_FORMAT <- 1L
KEPT_HEADERS <- c("x-plasticdb-release", "x-plasticdb-release-source",
    "x-plasticdb-snapshot-unavailable", "content-type", "content-disposition", "link")

pdb_bfc <- function(client) {
    dir <- client$cache_dir
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    BiocFileCache::BiocFileCache(dir, ask = FALSE)
}

cache_key_for <- function(method, url) {
    parts <- strsplit(url, "?", fixed = TRUE)[[1]]
    query <- if (length(parts) > 1L) {
        sort(strsplit(paste(parts[-1], collapse = "?"), "&", fixed = TRUE)[[1]])
    } else {
        character()
    }
    canonical <- jsonlite::toJSON(list(CACHE_FORMAT, toupper(method),
        parts[[1]], as.list(query)), auto_unbox = TRUE)
    hex(openssl::sha256(as.character(canonical)))
}

cache_rname <- function(release, key, suffix = "") {
    paste0(release, ":", key, suffix)
}

cache_find <- function(bfc, rname) {
    hits <- BiocFileCache::bfcquery(bfc, rname, field = "rname", exact = TRUE)
    if (!nrow(hits)) {
        return(NULL)
    }
    list(rid = hits$rid[[1]], path = BiocFileCache::bfcrpath(bfc,
        rids = hits$rid[[1]]))
}

cache_drop <- function(bfc, rname) {
    hits <- BiocFileCache::bfcquery(bfc, rname, field = "rname", exact = TRUE)
    if (nrow(hits)) BiocFileCache::bfcremove(bfc, hits$rid)
}

cache_new <- function(bfc, rname, ext) {
    cache_drop(bfc, rname)
    BiocFileCache::bfcnew(bfc, rname, ext = ext)
}

hex <- function(hash) {
    out <- as.character(hash)
    attributes(out) <- NULL
    out
}

sha256_bytes <- function(bytes) {
    hex(openssl::sha256(bytes))
}

sha256_file <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con))
    hex(openssl::sha256(con))
}

kept_headers <- function(headers) {
    headers[intersect(KEPT_HEADERS, names(headers))]
}

cache_get <- function(client, release, key) {
    tryCatch({
        bfc <- pdb_bfc(client)
        hit <- cache_find(bfc, cache_rname(release, key))
        if (is.null(hit)) return(NULL)
        meta <- readRDS(hit$path)
        if (!identical(meta$format, CACHE_FORMAT) ||
            !identical(meta$size, length(meta$body)) ||
            !identical(sha256_bytes(meta$body), meta$sha256)) {
            return(NULL)
        }
        meta
    }, error = function(e) NULL)
}

cache_put <- function(client, release, key, status, headers, bytes, url) {
    tryCatch({
        bfc <- pdb_bfc(client)
        path <- cache_new(bfc, cache_rname(release, key), ".rds")
        saveRDS(list(format = CACHE_FORMAT, status = status, url = url,
            headers = kept_headers(headers), size = length(bytes),
            sha256 = sha256_bytes(bytes), body = bytes), path)
        invisible(TRUE)
    }, error = function(e) invisible(FALSE))
}

# A cached download: its metadata and the path of its verified body.
cache_get_file <- function(client, release, key) {
    tryCatch({
        bfc <- pdb_bfc(client)
        meta_hit <- cache_find(bfc, cache_rname(release, key))
        body_hit <- cache_find(bfc, cache_rname(release, key, ":body"))
        if (is.null(meta_hit) || is.null(body_hit)) return(NULL)
        meta <- readRDS(meta_hit$path)
        if (!identical(meta$format, CACHE_FORMAT) ||
            !isTRUE(file.size(body_hit$path) == meta$size) ||
            !identical(sha256_file(body_hit$path), meta$sha256)) {
            return(NULL)
        }
        meta$path <- body_hit$path
        meta
    }, error = function(e) NULL)
}

cache_put_file <- function(client, release, key, source, status, headers,
                           url, sha256) {
    tryCatch({
        bfc <- pdb_bfc(client)
        ext <- tools::file_ext(source)
        body <- cache_new(bfc, cache_rname(release, key, ":body"),
            if (nzchar(ext)) paste0(".", ext) else "")
        file.copy(source, body, overwrite = TRUE)
        meta <- cache_new(bfc, cache_rname(release, key), ".rds")
        saveRDS(list(format = CACHE_FORMAT, status = status, url = url,
            headers = kept_headers(headers), size = file.size(source),
            sha256 = sha256), meta)
        invisible(TRUE)
    }, error = function(e) invisible(FALSE))
}
