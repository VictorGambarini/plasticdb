# The API's one query vocabulary (the server's docs/api-conventions.md), as
# named arguments in `...`. Names are the server's, so a filter means the same
# thing on every function and in the API reference. The rank `class` may also
# be spelled `class_`, as in the Python client.

PROTEIN_FILTERS <- c("q", "name", "plasticdb_id", "accession", "plastic",
    "include_subtypes", "microorganism", "lineage", "year", "min_year",
    "max_year", "substrate_form", "evidence_method", "include_negative",
    "polarity", "activity_class", "min_kcat", "max_km", "min_kcat_km",
    "min_vmax", "has_structure", "has_sequence", "ec_number", "min_tm",
    "max_tm", "origin_type", "origin_environment", "kind")
PROTEIN_LIST_FILTERS <- c(PROTEIN_FILTERS, "include_sequenceless",
    "include_sequence", "sort_by", "sort_order", "updated_since")
VARIANT_LIST_FILTERS <- c(setdiff(PROTEIN_FILTERS, c("kind", "plasticdb_id")),
    "include_sequenceless", "include_sequence", "sort_by", "sort_order",
    "updated_since")

MICROORGANISM_FILTERS <- c("q", "name", "plastic", "include_subtypes",
    "plastic_id", "tax_id", "year", "min_year", "max_year", "temp_class",
    "environment", "location", "lineage", "domain", "kingdom", "phylum",
    "class", "order", "family", "genus", "evidence_method", "substrate_form",
    "sole_carbon_source", "aerobicity", "include_negative", "polarity",
    "evidence_scope")
MICROORGANISM_LIST_FILTERS <- c(MICROORGANISM_FILTERS, "sort_by",
    "sort_order", "updated_since")

PLASTIC_FILTERS <- c("q", "name", "cas_number", "plastic_id", "polymer_class",
    "parent_id", "has_evidence")
PLASTIC_LIST_FILTERS <- c(PLASTIC_FILTERS, "evidence_scope", "sort_by",
    "sort_order", "updated_since")

PAPER_FILTERS <- c("q", "year", "min_year", "max_year", "doi", "journal",
    "authors", "country", "plastic", "include_subtypes", "microorganism",
    "protein", "evidence_method", "polarity", "has_quantitative")
PAPER_LIST_FILTERS <- c(PAPER_FILTERS, "sort_by", "sort_order",
    "updated_since")
PAPER_EXPORT_FILTERS <- c(PAPER_FILTERS, "updated_since")

VARIANT_DOWNLOAD_FILTERS <- c("parent_protein_id", "parent_plasticdb_id",
    "variant_type", "engineering_method")
MATERIAL_FILTERS <- c("protein", "protein_id", "protein_variant_id",
    "paper_id", "doi")
BINDING_ASSAY_FILTERS <- c(MATERIAL_FILTERS, "plastic", "plastic_id",
    "include_subtypes")

# The evidence lists' filters; the deeper lists add their parents' ids.
INTERACTION_FILTERS <- c("plastic", "include_subtypes", "plastic_id",
    "microorganism", "protein", "evidence_level", "paper_id", "doi",
    "polarity", "evidence_method", "substrate_form", "measurement_type",
    "has_quantitative")
EXPERIMENT_FILTERS <- c(INTERACTION_FILTERS, "interaction_id")
OBSERVATION_FILTERS <- c(EXPERIMENT_FILTERS, "experiment_id")

# The filters in `...` as query parameters; an unknown name is an error.
check_filters <- function(filters, allowed, where) {
    if (!length(filters)) {
        return(list())
    }
    given <- names(filters)
    if (is.null(given) || any(!nzchar(given))) {
        arg_error(sprintf("%s(): every filter must be named.", where))
    }
    given[given == "class_"] <- "class"
    names(filters) <- given
    unknown <- setdiff(given, allowed)
    if (length(unknown)) {
        arg_error(c(
            sprintf("%s() got unknown filter(s): %s.", where,
                paste(unknown, collapse = ", ")),
            i = paste("Valid filters:", paste(sort(allowed), collapse = ", "))))
    }
    Filter(Negate(is.null), filters)
}
