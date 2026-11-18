MAX_PAGE_SIZE <- 500L

# The provenance a result carries as attribute "plasticdb".
response_provenance <- function(client, result, total = NULL) {
    list(release = result$release, release_source = result$release_source,
        pinned = identical(result$release_source, "snapshot"),
        requested_release = client$release, url = result$url,
        from_cache = result$from_cache,
        unavailable_fields = result$unavailable_fields,
        retrieved_at = result$retrieved_at, total = total)
}

with_provenance <- function(x, client, result, total = NULL) {
    attr(x, "plasticdb") <- response_provenance(client, result, total)
    x
}

check_limit <- function(limit) {
    if (is.null(limit)) {
        return(Inf)
    }
    if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) ||
        limit < 0) {
        arg_error("`limit` must be a number >= 0 (or Inf for every record).")
    }
    limit
}

# Every item of a {total, items} list endpoint, up to `limit`, one page at a
# time. Returns list(items, total, first, envelope).
fetch_pages <- function(client, endpoint, params = list(),
                        path_params = list(), limit = Inf,
                        max_page_size = MAX_PAGE_SIZE) {
    limit <- check_limit(limit)
    items <- list()
    offset <- 0L
    first <- NULL
    total <- NULL
    envelope <- list()
    repeat {
        size <- max_page_size
        if (is.finite(limit)) size <- max(1L, min(size, limit - offset))
        page_params <- c(params, list(offset = offset, limit = size))
        result <- pdb_request(client, endpoint, page_params, path_params)
        data <- result$data
        if (!is_object(data) || !all(c("items", "total") %in% names(data))) {
            pdb_abort("unexpected_response", sprintf(
                "%s did not answer a {total, items} envelope", endpoint$path),
                url = result$url)
        }
        if (is.null(first)) {
            first <- result
            envelope <- data[setdiff(names(data), "items")]
        } else if (!identical(result$release, first$release)) {
            rlang::warn(sprintf(paste("PlasticDB: release changed from %s to",
                "%s while paging %s; pin a release for a consistent result."),
                first$release %||% "none", result$release %||% "none",
                endpoint$path), class = "plasticdb_release_changed")
        }
        total <- as.numeric(data$total)
        page <- data$items
        if (limit == 0) page <- list()
        items <- c(items, page)
        offset <- offset + length(page)
        wanted <- min(total, limit)
        if (!length(page) || length(items) >= wanted || offset >= total) {
            break
        }
    }
    if (is.finite(limit) && length(items) > limit) {
        items <- items[seq_len(limit)]
    }
    list(items = items, total = total, first = first, envelope = envelope)
}

# A list endpoint as a tibble with provenance (attr "plasticdb") attached.
list_tibble <- function(client, endpoint, params = list(),
                        path_params = list(), limit = Inf, kind = NULL,
                        level = "interaction",
                        max_page_size = MAX_PAGE_SIZE) {
    pages <- fetch_pages(client, endpoint, params, path_params, limit,
        max_page_size)
    out <- records_to_tibble(pages$items, kind, level)
    with_provenance(out, client, pages$first, pages$total)
}

# A record endpoint as a list with provenance attached.
get_record <- function(client, endpoint, params = list(),
                       path_params = list(), ...) {
    result <- pdb_request(client, endpoint, params, path_params, ...)
    as_record(result$data, client, result)
}

as_record <- function(data, client, result) {
    if (is.null(data)) data <- list()
    class(data) <- c("plasticdb_record", class(data))
    with_provenance(data, client, result)
}

#' @export
print.plasticdb_record <- function(x, ...) {
    prov <- attr(x, "plasticdb")
    cat("<plasticdb_record>", if (!is.null(prov$release))
        sprintf("(release %s, %s)", prov$release, prov$release_source), "\n")
    fields <- unclass(x)
    attr(fields, "plasticdb") <- NULL
    for (name in names(fields)) {
        value <- fields[[name]]
        shown <- if (is.null(value)) {
            "NULL"
        } else if (is.atomic(value) && length(value) == 1L) {
            text <- as.character(value)
            if (nchar(text) > 60) paste0(substr(text, 1, 57), "...") else text
        } else if (is_object(value)) {
            sprintf("<object: %d fields>", length(value))
        } else {
            sprintf("<list: %d>", length(value))
        }
        cat(sprintf("  $%s: %s\n", name, shown))
    }
    invisible(x)
}
