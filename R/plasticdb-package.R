#' plasticdb: client for the PlasticDB API
#'
#' PlasticDB (<https://v2.plasticdb.org>) is a curated database of
#' microorganisms and proteins linked to plastic biodegradation, and of the
#' evidence behind each link. This package reads it: catalogue tables as
#' tibbles, sequences as [Biostrings::AAStringSet]s, and files on disk.
#'
#' Start with [plasticdb_client()] (or the `plasticdb.release` option) to pin
#' a dataset release, then [proteins_list()], [microorganisms_list()],
#' [plastics_list()], [papers_list()], [evidence_interactions()] and
#' [search_hits()]. [provenance()] says what answered, and [cite()] how to
#' cite it. Errors are classed conditions: see [plasticdb-conditions].
#'
#' The code is MIT-licensed. **The PlasticDB data is licensed CC BY 4.0**:
#' cite the dataset release you used ([cite()]) and the PlasticDB paper.
#'
#' Assisted-by: Claude (Anthropic). Most of the code, tests and documentation
#' were written with the Claude Code assistant, directed and reviewed by the
#' maintainer, who is responsible for them.
#'
#' @keywords internal
"_PACKAGE"
