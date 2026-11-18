#' Download files
#'
#' Filtered tables, the whole dataset, and a release's checksummed
#' artefacts. Every function writes to `path` (a file, or an existing
#' directory to save the server's file name in) and returns the path written.
#'
#' * `downloads_proteins()`, `downloads_microorganisms()`,
#'   `downloads_plastics()`, `downloads_papers()` and
#'   `downloads_protein_variants()` save one table as CSV or JSON (proteins
#'   also as FASTA), filtered as the matching `*_list()` function. A pinned
#'   client downloads that release's table as published (any filter is then
#'   `plasticdb_release_unsupported_filter`); the file is kept in the
#'   BiocFileCache, so a second call is a local copy.
#' * `downloads_dataset()` saves every public record: a ZIP of CSVs with a
#'   README and licence, or one JSON (live only). The ZIP is built on demand;
#'   while it is being built the server answers 503 with `Retry-After`
#'   (`plasticdb_service_unavailable`, field `retry_after`); `max_wait`
#'   (seconds) keeps waiting that long for it.
#' * `downloads_search()` saves every record matching a search (live only).
#' * `downloads_artefacts()` lists the files a release published (Parquet
#'   tables, reference databases, `MANIFEST.json`);
#'   `downloads_release_file()` saves one, checked against the SHA-256 the
#'   release published (a mismatch is `plasticdb_checksum` and nothing is
#'   kept); `downloads_manifest()` returns the parsed `MANIFEST.json`; and
#'   `downloads_structure()` saves one structure's coordinates as published
#'   by a release (gzipped PDB). Release artefacts are immutable, so they are
#'   always cached. `release` defaults to the client's pinned release, else
#'   the current one.
#'
#' @inheritParams proteins
#' @param path A file, or an existing directory to save the file in.
#' @param format The file format.
#' @param ... Filters, as for the matching `*_list()` function.
#' @param max_wait Seconds to keep waiting while the dataset ZIP is built.
#' @param q The search text.
#' @param file An artefact's path within the release, e.g.
#'   `"parquet/proteins.parquet"`.
#' @param release A release version, or `NULL` for the pinned or current one.
#' @param structure_id A structure's numeric id.
#'
#' @return The path written (`downloads_artefacts()`: a tibble;
#'   `downloads_manifest()`: a list).
#' @name downloads
#' @examples
#' \dontrun{
#' downloads_proteins(tempdir(), format = "fasta", plastic = "PET")
#' downloads_dataset(tempdir(), max_wait = 300)
#' client <- plasticdb_client(release = "2026.50")
#' downloads_artefacts(client = client)
#' downloads_release_file("parquet/proteins.parquet", tempdir(),
#'     client = client)
#' }
NULL

download_table <- function(client, table, format, path, params) {
    endpoint <- ENDPOINTS[[paste0("dl_", table, "_", format)]]
    if (is.null(endpoint)) {
        arg_error(sprintf("%s downloads do not come as %s.", table, format))
    }
    pdb_download(client, endpoint, path, params = params,
        default_name = sprintf("plasticdb_%s.%s", gsub("-", "_", table),
            format))
}

#' @rdname downloads
#' @export
downloads_proteins <- function(path, format = c("csv", "json", "fasta"), ...,
                               client = plasticdb_client()) {
    params <- check_filters(list(...), PROTEIN_FILTERS, "downloads_proteins")
    download_table(client, "proteins", match.arg(format), path, params)
}

#' @rdname downloads
#' @export
downloads_microorganisms <- function(path, format = c("csv", "json"), ...,
                                     client = plasticdb_client()) {
    params <- check_filters(list(...), MICROORGANISM_FILTERS,
        "downloads_microorganisms")
    download_table(client, "microorganisms", match.arg(format), path, params)
}

#' @rdname downloads
#' @export
downloads_plastics <- function(path, format = c("csv", "json"), ...,
                               client = plasticdb_client()) {
    params <- check_filters(list(...), PLASTIC_FILTERS, "downloads_plastics")
    download_table(client, "plastics", match.arg(format), path, params)
}

#' @rdname downloads
#' @export
downloads_papers <- function(path, format = c("csv", "json"), ...,
                             client = plasticdb_client()) {
    params <- check_filters(list(...), PAPER_FILTERS, "downloads_papers")
    download_table(client, "papers", match.arg(format), path, params)
}

#' @rdname downloads
#' @export
downloads_protein_variants <- function(path, format = c("csv", "json"), ...,
                                       client = plasticdb_client()) {
    params <- check_filters(list(...), VARIANT_DOWNLOAD_FILTERS,
        "downloads_protein_variants")
    download_table(client, "protein-variants", match.arg(format), path, params)
}

#' @rdname downloads
#' @export
downloads_dataset <- function(path, format = c("zip", "json"), max_wait = 0,
                              client = plasticdb_client()) {
    format <- match.arg(format)
    endpoint <- ENDPOINTS[[paste0("dl_dataset_", format)]]
    deadline <- Sys.time() + max_wait
    repeat {
        out <- tryCatch(
            pdb_download(client, endpoint, path,
                default_name = paste0("plasticdb_dataset.", format)),
            plasticdb_service_unavailable = function(e) e)
        if (!inherits(out, "plasticdb_service_unavailable")) {
            return(out)
        }
        wait <- out$retry_after %||% 0
        if (inherits(out, "plasticdb_release_error") || !wait ||
            Sys.time() + wait > deadline) {
            rlang::cnd_signal(out)
        }
        pdb_sleep(wait)
    }
}

#' @rdname downloads
#' @export
downloads_search <- function(path, q, format = c("json", "zip"),
                             client = plasticdb_client()) {
    format <- match.arg(format)
    pdb_download(client, ENDPOINTS[[paste0("dl_search_", format)]], path,
        params = list(q = q), default_name = paste0("plasticdb_search.", format))
}

release_version <- function(client, release) {
    version <- release %||% client$release %||%
        releases_current(client = client)$version
    if (is.null(version)) {
        pdb_abort("release_error", paste("The server has no dataset release",
            "yet, so there are no release artefacts."), reason = "no_release")
    }
    version
}

release_artefacts <- function(client, version) {
    record <- releases_get(version, client = client)
    if (is.null(record$artefacts)) {
        pdb_abort("release_no_artefacts", sprintf(
            "Release %s has no artefacts", version), release = version,
            reason = "no_artefacts")
    }
    record$artefacts
}

#' @rdname downloads
#' @export
downloads_artefacts <- function(release = NULL, client = plasticdb_client()) {
    version <- release_version(client, release)
    artefacts <- release_artefacts(client, version)
    records_to_tibble(artefacts$files %||% list())
}

manifest_sha <- function(manifest, file) {
    for (entry in manifest$files %||% list()) {
        if (is_object(entry) && identical(entry$path, file)) {
            return(if (is.character(entry$sha256)) entry$sha256 else NULL)
        }
    }
    pdb_abort("not_found", sprintf("%s is not in the release manifest", file))
}

#' @rdname downloads
#' @export
downloads_release_file <- function(file, path, release = NULL,
                                   client = plasticdb_client()) {
    version <- release_version(client, release)
    artefacts <- release_artefacts(client, version)
    if (identical(file, "MANIFEST.json")) {
        sha <- artefacts$manifest_sha256
    } else {
        entry <- Filter(function(f) identical(f$path, file),
            artefacts$files %||% list())
        sha <- if (length(entry)) entry[[1]]$sha256 else
            manifest_sha(downloads_manifest(version, client = client), file)
    }
    pdb_download(client, ENDPOINTS$release_artefact, path, sha256 = sha,
        immutable_release = version, default_name = basename(file),
        path_params = list(version = version, path = file))
}

#' @rdname downloads
#' @export
downloads_manifest <- function(release = NULL, client = plasticdb_client()) {
    version <- release_version(client, release)
    dir <- tempfile("plasticdb-manifest-")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    target <- downloads_release_file("MANIFEST.json",
        file.path(dir, "MANIFEST.json"), release = version, client = client)
    jsonlite::read_json(target, simplifyVector = FALSE)
}

#' @rdname downloads
#' @export
downloads_structure <- function(structure_id, path, release = NULL,
                                client = plasticdb_client()) {
    version <- release_version(client, release)
    id <- as.integer(structure_id)
    sha <- manifest_sha(downloads_manifest(version, client = client),
        sprintf("structures/%d.pdb.gz", id))
    pdb_download(client, ENDPOINTS$release_structure, path, sha256 = sha,
        immutable_release = version, default_name = sprintf("%d.pdb.gz", id),
        path_params = list(version = version, structure_id = id))
}
