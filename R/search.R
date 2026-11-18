#' Full-text search
#'
#' Search every record type at once (live only: a past release cannot be
#' searched, so a pinned client gets `plasticdb_release_live_only`).
#'
#' * `search_hits()` returns ranked hits, one row each, with `entity_type`
#'   (`protein`, `microorganism`, `plastic`, `paper`, ...), `route_key` (the
#'   accession or id to pass to the matching `*_get()` function) and `rank`.
#'   (It is not called `search()`, which would mask [base::search()].)
#' * `search_grouped()` returns the top `limit` hits (at most 100) for each
#'   record type: a list with the overall `total`, a `groups` tibble (each
#'   type's total and the query that lists more) and the `hits`.
#' * `search_autocomplete()` returns name suggestions, as the site's search
#'   box shows them.
#'
#' @inheritParams proteins
#' @param q The search text.
#' @param limit For `search_hits()`, the most hits to return; for
#'   `search_grouped()`, the hits per record type.
#'
#' @return A tibble with provenance, or (`search_grouped()`) a list.
#' @name search_hits
#' @examples
#' \dontrun{
#' search_hits("cutinase", limit = 20)
#' search_grouped("PETase")$groups
#' }
NULL

#' @rdname search_hits
#' @export
search_hits <- function(q, limit = Inf, client = plasticdb_client()) {
    list_tibble(client, ENDPOINTS$search, list(q = q), limit = limit)
}

#' @rdname search_hits
#' @export
search_grouped <- function(q, limit = 10, client = plasticdb_client()) {
    result <- pdb_request(client, ENDPOINTS$search_grouped,
        list(q = q, limit = limit))
    data <- result$data %||% list()
    groups <- data$groups %||% list()
    summary <- tibble::tibble(
        group = names(groups),
        total = vapply(groups, function(g) as.numeric(g$total %||% NA), 0,
            USE.NAMES = FALSE),
        more_query = vapply(groups, function(g)
            as.character(g$more_query %||% NA), "", USE.NAMES = FALSE))
    hits <- unlist(lapply(names(groups), function(name) {
        lapply(groups[[name]]$items, function(item) c(list(group = name), item))
    }), recursive = FALSE)
    out <- list(total = data$total, scope = data$scope, groups = summary,
        hits = records_to_tibble(hits %||% list()))
    as_record(out, client, result)
}

#' @rdname search_hits
#' @export
search_autocomplete <- function(q, client = plasticdb_client()) {
    result <- pdb_request(client, ENDPOINTS$search_autocomplete, list(q = q))
    data <- result$data %||% list()
    rows <- unlist(lapply(names(data), function(kind) {
        if (!is.list(data[[kind]]) || is_object(data[[kind]])) return(list())
        lapply(data[[kind]], function(hit) c(list(entity_type = kind), hit))
    }), recursive = FALSE)
    with_provenance(records_to_tibble(rows %||% list()), client, result)
}

#' Degradation pathways
#'
#' Curated degradation pathways: ordered reaction steps from a polymer to
#' its metabolites.
#'
#' * `pathways_list()` lists every pathway; `pathways_get()` returns one with
#'   its steps; `pathways_for_plastic()` every pathway that degrades one
#'   plastic.
#' * `pathways_graph()` returns a plastic's pathways as a graph of compounds
#'   and reactions: a list of `nodes` and `edges` tibbles (each element's
#'   attributes, flattened), ready for
#'   `igraph::graph_from_data_frame(g$edges, vertices = g$nodes)`.
#'
#' @inheritParams proteins
#' @param pathway_id A pathway's id (in `pathways_graph()`, `NULL` for all
#'   of the plastic's pathways).
#' @param plastic_id A plastic's id (16 is PET).
#'
#' @return A tibble with provenance, a `plasticdb_record` list, or (for the
#'   graph) a list of two tibbles.
#' @name pathways
#' @examples
#' \dontrun{
#' pathways_list()
#' g <- pathways_graph(16)
#' g$edges
#' }
NULL

#' @rdname pathways
#' @export
pathways_list <- function(limit = Inf, client = plasticdb_client()) {
    list_tibble(client, ENDPOINTS$pathways_list, limit = limit,
        kind = "pathway")
}

#' @rdname pathways
#' @export
pathways_get <- function(pathway_id, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$pathways_get,
        path_params = list(pathway_id = pathway_id))
}

#' @rdname pathways
#' @export
pathways_for_plastic <- function(plastic_id, client = plasticdb_client()) {
    items_tibble(client, ENDPOINTS$pathways_for_plastic,
        list(plastic_id = plastic_id), kind = "pathway")
}

#' @rdname pathways
#' @export
pathways_graph <- function(plastic_id, pathway_id = NULL,
                           client = plasticdb_client()) {
    result <- pdb_request(client, ENDPOINTS$pathways_graph,
        list(pathway_id = pathway_id), list(plastic_id = plastic_id))
    data <- result$data %||% list()
    element_data <- function(elements) {
        lapply(elements %||% list(), function(e) e$data %||% e)
    }
    nodes <- records_to_tibble(element_data(data$nodes))
    edges <- records_to_tibble(element_data(data$edges))
    if (all(c("source", "target") %in% names(edges))) {
        first <- c("source", "target")
        edges <- edges[c(first, setdiff(names(edges), first))]
    }
    out <- list(nodes = nodes, edges = edges)
    with_provenance(out, client, result)
}

#' Database statistics
#'
#' Summary statistics of the live database (live only).
#'
#' * `stats_summary()`: headline counts (proteins, microorganisms, plastics,
#'   papers, pathways).
#' * `stats_protein_activity()`: how many proteins degrade, fail to degrade,
#'   or are untested.
#' * `stats_charts()`: the data behind the site's statistics charts, and
#'   `stats_by_country()` record counts by the corresponding author's
#'   country; both as a named list of tibbles.
#'
#' @inheritParams proteins
#' @param evidence_scope `"organism_or_enzyme"` or `"whole_organism"`.
#'
#' @return A `plasticdb_record` list, or a named list of tibbles.
#' @name stats_summary
#' @examples
#' \dontrun{
#' stats_summary()$proteins
#' stats_by_country()$microorganisms
#' }
NULL

#' @rdname stats_summary
#' @export
stats_summary <- function(evidence_scope = c("organism_or_enzyme",
                              "whole_organism"),
                          client = plasticdb_client()) {
    get_record(client, ENDPOINTS$stats,
        list(evidence_scope = match.arg(evidence_scope)))
}

#' @rdname stats_summary
#' @export
stats_protein_activity <- function(client = plasticdb_client()) {
    get_record(client, ENDPOINTS$stats_activity)
}

#' @rdname stats_summary
#' @export
stats_charts <- function(evidence_scope = c("organism_or_enzyme",
                             "whole_organism"),
                         client = plasticdb_client()) {
    result <- pdb_request(client, ENDPOINTS$stats_charts,
        list(evidence_scope = match.arg(evidence_scope)))
    with_provenance(tables_of(result$data), client, result)
}

#' @rdname stats_summary
#' @export
stats_by_country <- function(client = plasticdb_client()) {
    result <- pdb_request(client, ENDPOINTS$stats_country)
    with_provenance(tables_of(result$data), client, result)
}

# A JSON object of arrays: each array of objects becomes a tibble.
tables_of <- function(data) {
    lapply(data %||% list(), function(value) {
        if (is.list(value) && !is_object(value) &&
            all(vapply(value, is_object, TRUE))) {
            records_to_tibble(value)
        } else {
            value
        }
    })
}

#' The change feed
#'
#' Records updated, merged or removed at or after `since` (live only). Each
#' row has `entity_type`, `id`, `accession`, `status` (`"updated"`,
#' `"merged"` or `"removed"`), `merged_into` (a merge's survivor) and
#' `updated_at`. To keep a local copy in sync, store
#' `provenance(feed)$as_of` and pass it as the next `since`.
#'
#' @inheritParams proteins
#' @param since A time (POSIXct, Date or ISO 8601 string).
#' @param entity_type `NULL` for every type, or one of `"protein"`,
#'   `"protein_variant"`, `"microorganism"`, `"plastic"`, `"paper"`.
#'
#' @return A tibble; its provenance also carries `since` and `as_of`.
#' @export
#' @examples
#' \dontrun{
#' feed <- changes(since = Sys.Date() - 7)
#' table(feed$status)
#' provenance(feed)$as_of
#' }
changes <- function(since, entity_type = NULL, limit = Inf,
                    client = plasticdb_client()) {
    if (!is.null(entity_type)) {
        entity_type <- match.arg(entity_type, c("protein", "protein_variant",
            "microorganism", "plastic", "paper"))
    }
    pages <- fetch_pages(client, ENDPOINTS$changes,
        list(since = since, entity_type = entity_type), limit = limit,
        max_page_size = 5000L)
    out <- with_provenance(records_to_tibble(pages$items), client,
        pages$first, pages$total)
    prov <- attr(out, "plasticdb")
    prov$since <- pages$envelope$since
    prov$as_of <- pages$envelope$as_of
    attr(out, "plasticdb") <- prov
    out
}
