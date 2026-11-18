DATA_LICENSE <- "CC BY 4.0"
DATA_LICENSE_URL <- "https://creativecommons.org/licenses/by/4.0/"
PAPER_CITATION <- paste(
    "Gambarini V, Pantos O, Kingsbury JM, Weaver L, Handley KM, Lear G (2022).",
    "PlasticDB: a database of microorganisms and proteins linked to plastic",
    "biodegradation. Database, 2022:baac008.")
PAPER_DOI <- "10.1093/database/baac008"

#' Where results came from
#'
#' Record this alongside an analysis: the dataset release that answered, its
#' source (`"snapshot"` for a past release, `"live"` for the live database,
#' which also serves the current release), whether the client was pinned,
#' the server and package versions, and the data licence.
#'
#' Given a result (a tibble, record or AAStringSet from this package),
#' `provenance()` returns what answered that call, including the request
#' URL, whether it came from the cache, and the server's total for a list.
#' Without one it describes the client, making a request first when this
#' session has made none, so the release is known.
#'
#' @param x A result from this package, or `NULL` for the client's.
#' @param client A [plasticdb_client()].
#'
#' @return A named list.
#' @seealso [cite()], [last_response()]
#' @export
#' @examples
#' tbl <- tibble::tibble(id = 1L)
#' attr(tbl, "plasticdb") <- list(release = "2026.50",
#'     release_source = "snapshot", pinned = TRUE)
#' provenance(tbl)$release
provenance <- function(x = NULL, client = plasticdb_client()) {
    client <- check_client(client)
    common <- list(base_url = client$base_url,
        sdk_version = as.character(utils::packageVersion("plasticdb")),
        data_license = DATA_LICENSE)
    if (!is.null(x)) {
        prov <- if (methods::is(x, "Vector")) {
            S4Vectors::metadata(x)$plasticdb
        } else {
            attr(x, "plasticdb")
        }
        if (is.null(prov)) {
            arg_error("`x` carries no PlasticDB provenance.")
        }
        return(c(prov, common))
    }
    if (is.null(.state$last)) {
        if (!is.null(client$release)) {
            releases_get(client$release, client = client)
        } else {
            releases_current(client = client)
        }
    }
    read <- .state$last_read %||% .state$last
    release <- client$release %||% read$release
    source <- if (!is.null(.state$last_read)) {
        .state$last_read$release_source
    } else if (is.null(client$release)) {
        "live"
    }
    c(list(release = release, release_source = source,
        pinned = !is.null(client$release)), common,
        list(server_version = server_version(client)))
}

#' The server's application version
#'
#' @param client A [plasticdb_client()].
#' @return A string, or `NULL` when the server cannot be reached.
#' @export
#' @examples
#' server_version()
server_version <- function(client = plasticdb_client()) {
    client <- check_client(client)
    known <- .state$server_version[[client$base_url]]
    if (!is.null(known)) {
        return(known)
    }
    data <- tryCatch(pdb_request(client, ENDPOINTS$health, record = FALSE)$data,
        plasticdb_error = function(e) NULL)
    version <- if (is_object(data) && is.character(data$version))
        data$version
    if (!is.null(version)) {
        .state$server_version[[client$base_url]] <- version
    }
    version
}

#' How to cite PlasticDB
#'
#' The citation for the data a client reads: the dataset release (with its
#' DOI) and the PlasticDB paper, as a [utils::bibentry()] (so `print()`,
#' `format(style = "text")` and `toBibtex()` all work). The data is licensed
#' CC BY 4.0, which the citation states. A pinned client cites its release;
#' an unpinned one the current release. If the server cannot be reached the
#' citation still names the paper (with a warning).
#'
#' To cite this R package itself, use `citation("plasticdb")`.
#'
#' @param client A [plasticdb_client()].
#'
#' @return A `bibentry` with two entries: the dataset and the paper.
#' @export
#' @examples
#' ref <- cite()
#' ref
#' toBibtex(ref)
cite <- function(client = plasticdb_client()) {
    client <- check_client(client)
    rel <- tryCatch({
        if (!is.null(client$release)) {
            releases_get(client$release, client = client)
        } else {
            releases_current(client = client)
        }
    }, plasticdb_connection_error = function(e) e,
        plasticdb_server_error = function(e) {
            if (inherits(e, "plasticdb_release_error")) rlang::cnd_signal(e)
            e
        })
    if (inherits(rel, "condition")) {
        rlang::warn(sprintf(
            "PlasticDB: could not fetch the release citation (%s).",
            conditionMessage(rel)), class = "plasticdb_citation_offline")
        return(citation_entries(client, version = client$release, doi = NULL,
            text = NULL, live = is.null(client$release)))
    }
    citation_entries(client, version = rel$version, doi = rel$doi,
        text = rel$citation, live = is.null(rel$version),
        released_at = rel$released_at,
        license = rel$license %||% DATA_LICENSE,
        license_url = rel$license_url %||% DATA_LICENSE_URL)
}

citation_entries <- function(client, version, doi, text, live,
                                released_at = NULL, license = DATA_LICENSE,
                                license_url = DATA_LICENSE_URL) {
    year <- if (!is.null(released_at)) substr(released_at, 1, 4) else
        format(Sys.Date(), "%Y")
    if (live || is.null(version)) {
        title <- "PlasticDB (live database; no dataset release named)"
        text <- sprintf(paste("PlasticDB (live database; no dataset release",
            "named). University of Auckland. %s/ (accessed %s)"),
            client$root_url, format(Sys.Date()))
        note <- sprintf("Accessed %s", format(Sys.Date()))
    } else {
        title <- sprintf("PlasticDB dataset release %s", version)
        note <- sprintf("Dataset release %s", version)
        if (is.null(text)) {
            text <- sprintf(
                "PlasticDB dataset release %s. University of Auckland.",
                version)
            if (!is.null(doi)) text <- paste0(text, " https://doi.org/", doi)
        }
    }
    dataset <- utils::bibentry(
        bibtype = "Misc", key = "plasticdb_dataset",
        title = title,
        author = utils::person("PlasticDB", role = "aut"),
        organization = "University of Auckland",
        year = year,
        doi = doi,
        url = if (is.null(doi)) paste0(client$root_url, "/") else NULL,
        note = paste0(note, ". Data licence: ", license, " (", license_url,
            ")."),
        textVersion = text)
    paper <- utils::bibentry(
        bibtype = "Article", key = "gambarini2022plasticdb",
        title = paste("PlasticDB: a database of microorganisms and proteins",
            "linked to plastic biodegradation"),
        author = c(utils::person("Victor", "Gambarini"),
            utils::person("Olga", "Pantos"),
            utils::person(c("Joanne", "M."), "Kingsbury"),
            utils::person("Louise", "Weaver"),
            utils::person(c("Kim", "M."), "Handley"),
            utils::person("Gavin", "Lear")),
        journal = "Database", year = "2022", volume = "2022",
        pages = "baac008", doi = PAPER_DOI,
        textVersion = paste0(PAPER_CITATION, " https://doi.org/", PAPER_DOI))
    out <- c(dataset, paper)
    attr(out, "mfooter") <- sprintf("Data licence: %s (%s).", license,
        license_url)
    out
}
