# Every API operation the package calls, declared once.
#
# The contract test (tests/testthat/test-contract.R) checks each entry against
# the vendored SDK OpenAPI document, so functions must go through an entry here
# rather than a hand-built path.
#
# pinnable:    the client's release is sent as ?release=.
# live_only:   takes release but answers only the current one (a past release
#              is a 409 live_only). Still pinnable, so a pinned client gets that
#              error instead of a silently live answer.
# in_contract: FALSE for the few calls outside the SDK contract.
# raw_params:  path parameters that may contain "/".
# root:        resolve against the server root instead of /api/v1.

ep <- function(method, path, pinnable = TRUE, live_only = FALSE,
                in_contract = TRUE, raw_params = character(), root = FALSE) {
    if (live_only && !pinnable) {
        stop(path, ": live_only implies pinnable", call. = FALSE)
    }
    list(method = method, path = path, pinnable = pinnable,
        live_only = live_only, in_contract = in_contract,
        raw_params = raw_params, root = root)
}

ENDPOINTS <- list(
    health = ep("GET", "/health", pinnable = FALSE, in_contract = FALSE,
        root = TRUE),

    releases_list = ep("GET", "/releases", pinnable = FALSE),
    releases_current = ep("GET", "/releases/current", pinnable = FALSE),
    releases_status = ep("GET", "/releases/status", pinnable = FALSE),
    releases_get = ep("GET", "/releases/{version}", pinnable = FALSE),
    release_artefact = ep("GET", "/releases/{version}/artefacts/{path}",
        pinnable = FALSE, raw_params = "path"),
    release_structure = ep("GET",
        "/releases/{version}/structures/{structure_id}", pinnable = FALSE),

    proteins_list = ep("GET", "/proteins"),
    proteins_get = ep("GET", "/proteins/{accession}"),
    proteins_interactions = ep("GET", "/proteins/{accession}/interactions"),
    proteins_kinetics = ep("GET", "/proteins/{accession}/kinetics"),
    proteins_properties = ep("GET", "/proteins/{accession}/properties"),
    proteins_variants = ep("GET", "/proteins/{accession}/variants"),
    proteins_fasta = ep("GET", "/proteins/{ref}/fasta", live_only = TRUE),
    proteins_structure = ep("GET", "/proteins/{ref}/structure",
        live_only = TRUE),
    engineering_list = ep("GET", "/engineering/proteins", live_only = TRUE),
    engineering_get = ep("GET", "/engineering/proteins/{accession}",
        live_only = TRUE),

    variants_get = ep("GET", "/variants/{ref}", live_only = TRUE),
    variants_fasta = ep("GET", "/variants/{ref}/fasta", live_only = TRUE),
    variants_structure = ep("GET", "/variants/{ref}/structure",
        live_only = TRUE),

    microorganisms_list = ep("GET", "/microorganisms"),
    microorganisms_get = ep("GET", "/microorganisms/{ref}"),
    microorganisms_names = ep("GET", "/microorganisms/names",
        live_only = TRUE),
    microorganisms_levels = ep("GET", "/microorganisms/lineage-levels",
        live_only = TRUE),
    microorganisms_taxonomy = ep("GET",
        "/microorganisms/taxonomy/{rank}/{name}", live_only = TRUE),

    plastics_list = ep("GET", "/plastics"),
    plastics_get = ep("GET", "/plastics/{plastic_id}"),
    plastics_names = ep("GET", "/plastics/names", live_only = TRUE),

    papers_list = ep("GET", "/papers"),
    papers_get = ep("GET", "/papers/{paper_id}"),
    papers_footprint = ep("GET", "/papers/{paper_id}/footprint",
        live_only = TRUE),
    papers_export_all = ep("GET", "/papers/export", live_only = TRUE),
    papers_export_one = ep("GET", "/papers/{paper_id}/export",
        live_only = TRUE),

    binding_list = ep("GET", "/binding-assays"),
    binding_get = ep("GET", "/binding-assays/{assay_id}"),
    constructs_list = ep("GET", "/constructs"),
    constructs_get = ep("GET", "/constructs/{construct_id}"),
    constructs_structures = ep("GET", "/constructs/{construct_id}/structures"),
    preparations_list = ep("GET", "/preparations"),
    preparations_get = ep("GET", "/preparations/{preparation_id}"),
    experiment_preparations = ep("GET",
        "/experiments/{experiment_id}/preparations"),

    search = ep("GET", "/search", live_only = TRUE),
    search_grouped = ep("GET", "/search/grouped", live_only = TRUE),
    search_autocomplete = ep("GET", "/search/autocomplete", live_only = TRUE),

    pathways_list = ep("GET", "/pathways"),
    pathways_get = ep("GET", "/pathways/{pathway_id}"),
    pathways_for_plastic = ep("GET", "/pathways/by-plastic/{plastic_id}"),
    pathways_graph = ep("GET", "/pathways/by-plastic/{plastic_id}/graph"),

    stats = ep("GET", "/stats", live_only = TRUE),
    stats_activity = ep("GET", "/stats/protein-activity", live_only = TRUE),
    stats_charts = ep("GET", "/stats/charts", live_only = TRUE),
    stats_country = ep("GET", "/stats/by-country", live_only = TRUE),

    changes = ep("GET", "/changes", live_only = TRUE),

    dl_proteins_csv = ep("GET", "/downloads/proteins.csv"),
    dl_proteins_json = ep("GET", "/downloads/proteins.json"),
    dl_proteins_fasta = ep("GET", "/downloads/proteins.fasta"),
    dl_microorganisms_csv = ep("GET", "/downloads/microorganisms.csv"),
    dl_microorganisms_json = ep("GET", "/downloads/microorganisms.json"),
    dl_plastics_csv = ep("GET", "/downloads/plastics.csv"),
    dl_plastics_json = ep("GET", "/downloads/plastics.json"),
    dl_papers_csv = ep("GET", "/downloads/papers.csv"),
    dl_papers_json = ep("GET", "/downloads/papers.json"),
    `dl_protein-variants_csv` = ep("GET", "/downloads/protein-variants.csv"),
    `dl_protein-variants_json` = ep("GET",
        "/downloads/protein-variants.json"),
    dl_dataset_zip = ep("GET", "/downloads/dataset.zip", live_only = TRUE),
    dl_dataset_json = ep("GET", "/downloads/dataset.json", live_only = TRUE),
    dl_search_zip = ep("GET", "/downloads/search.zip", live_only = TRUE),
    dl_search_json = ep("GET", "/downloads/search.json", live_only = TRUE)
)

# The URL path of an endpoint with its path parameters filled in (escaped).
ep_format <- function(endpoint, path_params = list()) {
    fields <- regmatches(endpoint$path,
        gregexpr("\\{[A-Za-z_][A-Za-z0-9_]*\\}", endpoint$path))[[1]]
    fields <- gsub("[{}]", "", fields)
    missing <- setdiff(fields, names(path_params))
    if (length(missing)) {
        stop(endpoint$path, ": missing path parameter(s) ",
            paste(missing, collapse = ", "), call. = FALSE)
    }
    out <- endpoint$path
    for (name in fields) {
        value <- path_params[[name]]
        if (length(value) != 1L || is.na(value)) {
            rlang::abort(sprintf("`%s` must be a single value.", name),
                class = "plasticdb_argument_error")
        }
        text <- as.character(value)
        if (!nzchar(trimws(text))) {
            rlang::abort(sprintf("`%s` is empty.", name),
                class = "plasticdb_argument_error")
        }
        if (name %in% endpoint$raw_params) {
            parts <- strsplit(text, "/", fixed = TRUE)[[1]]
            text <- paste(vapply(parts, curl_escape, ""), collapse = "/")
        } else {
            text <- curl_escape(text)
        }
        out <- sub(paste0("{", name, "}"), text, out, fixed = TRUE)
    }
    out
}

curl_escape <- function(x) {
    utils::URLencode(x, reserved = TRUE)
}
