#' The evidence behind the catalogue
#'
#' Evidence is organised interaction -> experiment -> observation: one actor
#' (an organism or a protein) tested on one plastic in one paper, the
#' experiments that tested it, and the values each measured. Interactions are
#' listed per protein; `evidence_interactions(level = "observation")` gives
#' one row per measured value. The material tables describe what was assayed:
#' constructs (sequences as expressed), preparations (host, purification,
#' tags) and binding (adsorption) assays.
#'
#' * `evidence_interactions()`, `evidence_kinetics()` and
#'   `evidence_properties()` are the same as [proteins_interactions()],
#'   [proteins_kinetics()] and [proteins_properties()].
#' * `evidence_binding_assays()`, `evidence_constructs()` and
#'   `evidence_preparations()` list the material tables, filtered by
#'   `protein`, `protein_id`, `protein_variant_id`, `paper_id` or `doi` (and,
#'   for binding assays, `plastic`, `plastic_id` and `include_subtypes`).
#'   Constructs carry sequences: [as_aastringset()] works on them.
#' * `evidence_binding_assay()`, `evidence_construct()` and
#'   `evidence_preparation()` return one record by id;
#'   `evidence_construct_structures()` the structures linked to a construct
#'   and `evidence_experiment_preparations()` the preparations one experiment
#'   assayed.
#' * `interactions_list()`, `experiments_list()` and `observations_list()`
#'   list the spine as flat tables of every public record (the columns of
#'   the release tables of the same name; an interaction adds `actor_type`
#'   and the actors' accessions, `product_compound_ids` is a list column),
#'   and `interactions_get()`, `experiments_get()` and `observations_get()`
#'   return one by id. They need no protein: filter with `plastic` (and
#'   `include_subtypes`), `plastic_id`, `microorganism` (the evidence that
#'   credits the organism, its enzymes' assays included), `protein` (an
#'   accession), `evidence_level` (`"all"`, `"direct"` or `"indirect"`;
#'   `"direct"` and `"indirect"` need `protein` or `microorganism`),
#'   `paper_id`, `doi`, `polarity`, `evidence_method`, `substrate_form`,
#'   `measurement_type` and `has_quantitative`. Experiments also take
#'   `interaction_id`, and observations `interaction_id` and `experiment_id`.
#'   On interactions the experiment and observation filters ask one of its
#'   experiments to match them all. These lists answer a pinned release too.
#'   Records are visible exactly as in a release: staged, withdrawn or merged
#'   records are never listed.
#'
#' @inheritParams proteins
#' @param protein A protein accession.
#' @param assay_id,construct_id,preparation_id,experiment_id,interaction_id
#'   A record's id.
#' @param observation_id A record's id.
#'
#' @return A tibble with provenance, or a `plasticdb_record` list.
#' @name evidence
#' @examples
#' obs <- evidence_interactions("PBFCTW", level = "observation", limit = 2)
#' dim(obs)
#' \donttest{
#' interactions_list(plastic = "PET", evidence_level = "direct",
#'     protein = "PBFCTW", limit = 5)
#' constructs <- evidence_constructs(protein = "PBFCTW", limit = 2)
#' as_aastringset(constructs)
#' }
NULL

#' @rdname evidence
#' @export
evidence_interactions <- function(protein,
                                    evidence_level = c("all", "direct",
                                        "indirect"),
                                    level = c("interaction", "experiment",
                                        "observation"),
                                    include_sequenceless = FALSE, limit = Inf,
                                    kind = NULL,
                                    client = plasticdb_client()) {
    evidence_level <- resolve_evidence_level(evidence_level,
        missing(evidence_level), kind, "evidence_interactions")
    proteins_interactions(protein, evidence_level = evidence_level,
        level = level, include_sequenceless = include_sequenceless,
        limit = limit, client = client)
}

#' @rdname evidence
#' @export
evidence_kinetics <- function(protein, limit = Inf,
                                client = plasticdb_client()) {
    proteins_kinetics(protein, limit = limit, client = client)
}

#' @rdname evidence
#' @export
evidence_properties <- function(protein, limit = Inf,
                                client = plasticdb_client()) {
    proteins_properties(protein, limit = limit, client = client)
}

#' @rdname evidence
#' @export
evidence_binding_assays <- function(..., limit = Inf,
                                    client = plasticdb_client()) {
    params <- check_filters(list(...), BINDING_ASSAY_FILTERS,
        "evidence_binding_assays")
    list_tibble(client, ENDPOINTS$binding_list, params, limit = limit,
        kind = "binding_assay")
}

#' @rdname evidence
#' @export
evidence_binding_assay <- function(assay_id, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$binding_get,
        path_params = list(assay_id = assay_id))
}

#' @rdname evidence
#' @export
evidence_constructs <- function(..., limit = Inf,
                                client = plasticdb_client()) {
    params <- check_filters(list(...), MATERIAL_FILTERS,
        "evidence_constructs")
    list_tibble(client, ENDPOINTS$constructs_list, params, limit = limit)
}

#' @rdname evidence
#' @export
evidence_construct <- function(construct_id, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$constructs_get,
        path_params = list(construct_id = construct_id))
}

#' @rdname evidence
#' @export
evidence_construct_structures <- function(construct_id,
                                            client = plasticdb_client()) {
    items_tibble(client, ENDPOINTS$constructs_structures,
        list(construct_id = construct_id))
}

#' @rdname evidence
#' @export
evidence_preparations <- function(..., limit = Inf,
                                    client = plasticdb_client()) {
    params <- check_filters(list(...), MATERIAL_FILTERS,
        "evidence_preparations")
    list_tibble(client, ENDPOINTS$preparations_list, params, limit = limit)
}

#' @rdname evidence
#' @export
evidence_preparation <- function(preparation_id, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$preparations_get,
        path_params = list(preparation_id = preparation_id))
}

#' @rdname evidence
#' @export
evidence_experiment_preparations <- function(experiment_id,
                                                client = plasticdb_client()) {
    items_tibble(client, ENDPOINTS$experiment_preparations,
        list(experiment_id = experiment_id))
}

# An endpoint answering a plain array (or an {items} object) as a tibble.
items_tibble <- function(client, endpoint, path_params = list(),
                            params = list(), kind = NULL) {
    result <- pdb_request(client, endpoint, params, path_params)
    data <- result$data
    items <- if (is_object(data)) data$items %||% list() else data %||% list()
    with_provenance(records_to_tibble(items, kind), client, result)
}

#' @rdname evidence
#' @export
interactions_list <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), INTERACTION_FILTERS,
        "interactions_list")
    list_tibble(client, ENDPOINTS$interactions_list, params, limit = limit)
}

#' @rdname evidence
#' @export
interactions_get <- function(interaction_id, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$interactions_get,
        path_params = list(interaction_id = interaction_id))
}

#' @rdname evidence
#' @export
experiments_list <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), EXPERIMENT_FILTERS,
        "experiments_list")
    list_tibble(client, ENDPOINTS$experiments_list, params, limit = limit)
}

#' @rdname evidence
#' @export
experiments_get <- function(experiment_id, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$experiments_get,
        path_params = list(experiment_id = experiment_id))
}

#' @rdname evidence
#' @export
observations_list <- function(..., limit = Inf, client = plasticdb_client()) {
    params <- check_filters(list(...), OBSERVATION_FILTERS,
        "observations_list")
    list_tibble(client, ENDPOINTS$observations_list, params, limit = limit)
}

#' @rdname evidence
#' @export
observations_get <- function(observation_id, client = plasticdb_client()) {
    get_record(client, ENDPOINTS$observations_get,
        path_params = list(observation_id = observation_id))
}
