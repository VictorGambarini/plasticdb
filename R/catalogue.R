#' Microorganisms
#'
#' Microorganisms with evidence of acting on plastics.
#'
#' * `microorganisms_list()` lists organisms matching the filters in `...`
#'   (positive evidence unless `polarity =` says otherwise). Taxonomic ranks
#'   are filters too: `genus = "Pseudomonas"`, `class = "Bacilli"` (or
#'   `class_`, as in the Python client).
#' * `microorganisms_get()` returns one organism by NCBI tax-id or PlasticDB
#'   accession. `evidence_scope = "whole_organism"` counts only assays on the
#'   organism itself, not on enzymes purified from it. Merges behave as in
#'   [proteins_get()].
#' * `microorganisms_names()` is every organism's `id` and `name`,
#'   `microorganisms_lineage_levels()` the taxon names at each rank (narrowed
#'   by the ranks given), and `microorganisms_taxonomy()` aggregate statistics
#'   for one taxon (a renamed taxon is followed). These three are live only.
#'
#' @inheritParams proteins
#' @param ref An NCBI tax-id or a PlasticDB accession.
#' @param evidence_scope `"organism_or_enzyme"` (default) or
#'   `"whole_organism"`.
#' @param domain,kingdom,phylum,class,order,family Taxa to narrow the levels.
#' @param rank,name A rank (`"genus"`) and a taxon name (`"Pseudomonas"`).
#'
#' @return A tibble with provenance, or a `plasticdb_record` list.
#' @name microorganisms
#' @examples
#' \dontrun{
#' microorganisms_list(genus = "Pseudomonas", plastic = "PET")
#' microorganisms_get(1547922)$name
#' microorganisms_taxonomy("genus", "Ideonella")
#' }
NULL

#' @rdname microorganisms
#' @export
microorganisms_list <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), MICROORGANISM_LIST_FILTERS,
        "microorganisms_list")
    list_tibble(client, ENDPOINTS$microorganisms_list, params, limit = limit,
        kind = "microorganism")
}

#' @rdname microorganisms
#' @export
microorganisms_get <- function(ref,
                               evidence_scope = c("organism_or_enzyme",
                                   "whole_organism"),
                               follow_merges = FALSE,
                               client = plasticdb_client()) {
    evidence_scope <- match.arg(evidence_scope)
    current <- ref
    for (i in seq_len(MAX_MERGE_HOPS)) {
        out <- tryCatch(
            get_record(client, ENDPOINTS$microorganisms_get,
                list(evidence_scope = evidence_scope), list(ref = current)),
            plasticdb_merged = function(e) {
                if (!isTRUE(follow_merges)) rlang::cnd_signal(e)
                warn_merged(as.character(current), e$merged_into)
                structure(list(), next_ref = e$merged_into)
            })
        if (is.null(attr(out, "next_ref"))) {
            return(out)
        }
        current <- attr(out, "next_ref")
    }
    pdb_abort("unexpected_response", sprintf("%s: too many merges", ref))
}

#' @rdname microorganisms
#' @export
microorganisms_names <- function(client = plasticdb_client()) {
    result <- pdb_request(client, ENDPOINTS$microorganisms_names)
    with_provenance(records_to_tibble(result$data %||% list()), client, result)
}

#' @rdname microorganisms
#' @export
microorganisms_lineage_levels <- function(domain = NULL, kingdom = NULL,
                                          phylum = NULL, class = NULL,
                                          order = NULL, family = NULL,
                                          client = plasticdb_client()) {
    params <- list(domain = domain, kingdom = kingdom, phylum = phylum,
        class = class, order = order, family = family)
    result <- pdb_request(client, ENDPOINTS$microorganisms_levels, params)
    levels <- lapply(result$data %||% list(), function(x)
        as.character(unlist(x)))
    with_provenance(levels, client, result)
}

#' @rdname microorganisms
#' @export
microorganisms_taxonomy <- function(rank, name, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$microorganisms_taxonomy,
        path_params = list(rank = rank, name = name), follow_redirects = TRUE)
}

#' Plastics
#'
#' The plastic vocabulary, with the organisms and proteins acting on each.
#'
#' * `plastics_list()` lists plastics matching the filters in `...`
#'   (`has_evidence = TRUE`: only those with evidence).
#' * `plastics_get()` returns one plastic with one page each of its
#'   microorganisms and proteins. For every organism or protein acting on a
#'   plastic use `microorganisms_list(plastic_id = )` or
#'   `proteins_list(plastic = )` instead.
#' * `plastics_names()` is every plastic's `id` and `name` (live only).
#'
#' @inheritParams proteins
#' @param plastic_id A plastic's numeric id (16 is PET).
#' @param microorganism_limit,microorganism_offset,microorganism_sort_by,microorganism_sort_order,microorganism_polarity
#'   The page of organisms embedded in the record.
#' @param protein_limit,protein_offset,protein_sort_by,protein_sort_order,protein_polarity
#'   The page of proteins embedded in the record.
#' @param evidence_scope `"organism_or_enzyme"` or `"whole_organism"`.
#'
#' @return A tibble with provenance, or a `plasticdb_record` list.
#' @name plastics
#' @examples
#' \dontrun{
#' plastics_list(has_evidence = TRUE)
#' pet <- plastics_get(16, protein_limit = 10)
#' pet$name
#' }
NULL

#' @rdname plastics
#' @export
plastics_list <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), PLASTIC_LIST_FILTERS, "plastics_list")
    list_tibble(client, ENDPOINTS$plastics_list, params, limit = limit,
        kind = "plastic")
}

#' @rdname plastics
#' @export
plastics_get <- function(plastic_id, microorganism_limit = 25,
                         microorganism_offset = 0,
                         microorganism_sort_by = c("papers", "result", "name"),
                         microorganism_sort_order = c("desc", "asc"),
                         microorganism_polarity = c("positive", "negative",
                             "inconclusive"),
                         protein_limit = 25, protein_offset = 0,
                         protein_sort_by = c("id", "papers", "result",
                             "plasticdb_id", "accession", "name"),
                         protein_sort_order = c("desc", "asc"),
                         protein_polarity = c("positive", "negative",
                             "inconclusive"),
                         evidence_scope = c("organism_or_enzyme",
                             "whole_organism"),
                         client = plasticdb_client()) {
    params <- list(
        microorganism_limit = microorganism_limit,
        microorganism_offset = microorganism_offset,
        microorganism_sort_by = match.arg(microorganism_sort_by),
        microorganism_sort_order = match.arg(microorganism_sort_order),
        microorganism_polarity = match.arg(microorganism_polarity),
        protein_limit = protein_limit,
        protein_offset = protein_offset,
        protein_sort_by = match.arg(protein_sort_by),
        protein_sort_order = match.arg(protein_sort_order),
        protein_polarity = match.arg(protein_polarity),
        evidence_scope = match.arg(evidence_scope))
    get_record(client, ENDPOINTS$plastics_get, params,
        list(plastic_id = plastic_id))
}

#' @rdname plastics
#' @export
plastics_names <- function(client = plasticdb_client()) {
    result <- pdb_request(client, ENDPOINTS$plastics_names)
    with_provenance(records_to_tibble(result$data %||% list()), client, result)
}

#' Papers
#'
#' The literature behind the evidence.
#'
#' * `papers_list()` lists papers matching the filters in `...`
#'   (`polarity` has no default here: every paper).
#' * `papers_get()` returns one paper with the organisms, proteins and
#'   variants it reports.
#' * `papers_footprint()` is every record one paper contributed (live only).
#' * `papers_export()` returns citations as BibTeX or RIS: one paper, or
#'   every paper matching the filters (live only). With `path` it writes them
#'   to a file and returns the path.
#'
#' @inheritParams proteins
#' @param paper_id A paper's numeric id (`NULL` in `papers_export()` exports
#'   every paper matching the filters).
#' @param format `"bibtex"` or `"ris"`.
#' @param path A file or directory to write the export to, or `NULL` to
#'   return the text.
#'
#' @return A tibble with provenance, a `plasticdb_record` list, or text.
#' @name papers
#' @examples
#' \dontrun{
#' papers_list(protein = "PBFCTW", min_year = 2020)
#' papers_get(1262)$title
#' cat(papers_export(1262))
#' }
NULL

#' @rdname papers
#' @export
papers_list <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), PAPER_LIST_FILTERS, "papers_list")
    list_tibble(client, ENDPOINTS$papers_list, params, limit = limit)
}

#' @rdname papers
#' @export
papers_get <- function(paper_id, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$papers_get,
        path_params = list(paper_id = paper_id))
}

#' @rdname papers
#' @export
papers_footprint <- function(paper_id, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$papers_footprint,
        path_params = list(paper_id = paper_id))
}

#' @rdname papers
#' @export
papers_export <- function(paper_id = NULL, ..., format = c("bibtex", "ris"),
                          path = NULL, client = plasticdb_client()) {
    format <- match.arg(format)
    params <- list(format = format)
    filters <- list(...)
    if (!is.null(paper_id)) {
        if (length(filters)) {
            arg_error("papers_export(): pass a paper_id or filters, not both.")
        }
        endpoint <- ENDPOINTS$papers_export_one
        path_params <- list(paper_id = paper_id)
    } else {
        params <- c(params, check_filters(filters, PAPER_EXPORT_FILTERS,
            "papers_export"))
        endpoint <- ENDPOINTS$papers_export_all
        path_params <- list()
    }
    ext <- if (format == "bibtex") "bib" else "ris"
    if (!is.null(path)) {
        return(pdb_download(client, endpoint, path, params = params,
            path_params = path_params, default_name = paste0("papers.", ext)))
    }
    body_text(pdb_request(client, endpoint, params, path_params)$data)
}
