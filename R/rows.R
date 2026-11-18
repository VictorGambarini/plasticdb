# Flat tables from API records, as the Python client's rows():
# nested objects become `parent.child` columns, lists of plain values become
# list columns of vectors, and a few nested lists are summarised (a protein's
# degradation_links become plastics_positive / _negative / _inconclusive).

is_object <- function(x) is.list(x) && !is.null(names(x))

flatten_record <- function(x, prefix = "") {
    out <- list()
    for (key in names(x)) {
        value <- x[[key]]
        name <- paste0(prefix, key)
        if (is_object(value) && length(value)) {
            out <- c(out, flatten_record(value, paste0(name, ".")))
        } else if (is_object(value)) {
            out[name] <- list(NULL)
        } else {
            out[name] <- list(value)
        }
    }
    out
}

# Drop fields (and, with a vector value, sub-fields of a nested object).
drop_fields <- function(x, exclude) {
    for (name in names(exclude)) {
        rule <- exclude[[name]]
        if (isTRUE(rule)) {
            x[[name]] <- NULL
        } else if (is_object(x[[name]])) {
            for (sub in rule) x[[name]][[sub]] <- NULL
        }
    }
    x
}

pluck_chr <- function(items, field) {
    out <- vapply(items, function(i) {
        v <- i[[field]]
        if (is.null(v) || !length(v)) NA_character_ else as.character(v[[1]])
    }, "")
    unname(out[!is.na(out)])
}

link_columns <- function(links) {
    by <- function(count) {
        keep <- Filter(function(l) isTRUE((l[[count]] %||% 0) > 0), links)
        vapply(keep, function(l) as.character(l$plastic$name %||% NA), "")
    }
    list(plastics_positive = by("positive_count"),
        plastics_negative = by("negative_count"),
        plastics_inconclusive = by("inconclusive_count"))
}

paper_columns <- function(papers) {
    list(paper_ids = as.integer(unlist(lapply(papers, `[[`, "id"))),
        paper_dois = pluck_chr(papers, "doi"))
}

ORGANISM_NESTED <- c("degradation_links", "papers", "paper", "environments",
    "locations")

# Per record type: fields left out of the row, and summary columns added.
ROW_KINDS <- list(
    microorganism = list(
        exclude = list(degradation_links = TRUE, papers = TRUE),
        summary = function(x) c(link_columns(x$degradation_links),
            paper_columns(x$papers))),
    protein = list(
        exclude = list(degradation_links = TRUE, papers = TRUE,
            source_microorganism = ORGANISM_NESTED),
        summary = function(x) c(link_columns(x$degradation_links),
            paper_columns(x$papers))),
    variant = list(
        exclude = list(mutations = TRUE, kinetics = TRUE, properties = TRUE),
        summary = function(x) list(
            mutations = pluck_chr(x$mutations, "raw"),
            kinetics_count = length(x$kinetics),
            properties_count = length(x$properties))),
    plastic = list(
        exclude = list(children = TRUE),
        summary = function(x) list(children = pluck_chr(x$children, "name"))),
    interaction = list(
        exclude = list(experiments = TRUE),
        summary = function(x) list(
            methods = sort(unique(pluck_chr(x$experiments, "method"))),
            experiment_count = length(x$experiments),
            observation_count = sum(vapply(x$experiments,
                function(e) length(e$observations), 0L)))),
    binding_assay = list(
        exclude = list(measurements = TRUE, preparations = TRUE),
        summary = function(x) list(
            measurement_count = length(x$measurements),
            preparation_ids = as.integer(unlist(lapply(x$preparations,
                `[[`, "id"))))),
    pathway = list(
        exclude = list(steps = TRUE),
        summary = function(x) list(step_count = length(x$steps)))
)

record_row <- function(x, kind = NULL) {
    spec <- if (is.null(kind)) NULL else ROW_KINDS[[kind]]
    if (is.null(spec)) {
        return(flatten_record(x))
    }
    row <- flatten_record(drop_fields(x, spec$exclude))
    summary <- spec$summary(x)
    for (name in names(summary)) {
        value <- summary[[name]]
        # Summaries of lists stay list columns, even with one value per row.
        if (!endsWith(name, "_count")) value <- as.list(value)
        row[name] <- list(value)
    }
    row
}

# One interaction as rows per experiment or per observation.
interaction_rows <- function(x, level) {
    base <- record_row(x, "interaction")
    experiments <- x$experiments
    if (!length(experiments)) experiments <- list(NULL)
    rows <- list()
    for (exp in experiments) {
        exp_cols <- list()
        observations <- list(NULL)
        if (!is.null(exp)) {
            exp_cols <- flatten_record(drop_fields(exp,
                list(observations = TRUE)), "experiment.")
            if (level == "experiment") {
                exp_cols[["experiment.observation_count"]] <-
                    length(exp$observations)
            }
            if (length(exp$observations)) observations <- exp$observations
        }
        if (level == "experiment") {
            rows[[length(rows) + 1L]] <- c(base, exp_cols)
            next
        }
        for (obs in observations) {
            obs_cols <- if (is.null(obs)) list() else
                flatten_record(obs, "observation.")
            rows[[length(rows) + 1L]] <- c(base, exp_cols, obs_cols)
        }
    }
    rows
}

# A column from a list of values: atomic when every value is a scalar.
as_column <- function(values) {
    is_scalar <- vapply(values, function(v) is.null(v) ||
        (is.atomic(v) && length(v) == 1L), TRUE)
    if (all(is_scalar)) {
        present <- Filter(Negate(is.null), values)
        if (!length(present)) {
            return(rep(NA, length(values)))
        }
        types <- unique(vapply(present, typeof, ""))
        fill <- function(na, cast) {
            vapply(values, function(v) if (is.null(v)) na else cast(v), na)
        }
        if (identical(types, "logical")) return(fill(NA, as.logical))
        if (all(types %in% c("integer", "logical")) &&
            "integer" %in% types) return(fill(NA_integer_, as.integer))
        if (all(types %in% c("integer", "double", "logical"))) {
            return(fill(NA_real_, as.numeric))
        }
        if ("character" %in% types &&
            all(types %in% c("character", "integer", "double"))) {
            return(fill(NA_character_, as.character))
        }
    }
    lapply(values, function(v) {
        if (is.list(v) && length(v) && !is_object(v) &&
            all(vapply(v, function(i) is.atomic(i) && length(i) == 1L, TRUE))) {
            unlist(v)
        } else {
            v
        }
    })
}

# Rows (lists of column -> value) as a tibble with the same columns throughout.
rows_to_tibble <- function(rows) {
    if (!length(rows)) {
        return(tibble::tibble())
    }
    columns <- unique(unlist(lapply(rows, names)))
    nested <- unique(sub("\\..*$", "", columns[grepl(".", columns,
        fixed = TRUE)]))
    drop <- vapply(columns, function(col) col %in% nested &&
        all(vapply(rows, function(r) is.null(r[[col]]), TRUE)), TRUE)
    columns <- columns[!drop]
    data <- lapply(columns, function(col) {
        as_column(lapply(rows, function(r) r[[col]]))
    })
    names(data) <- columns
    tibble::as_tibble(data, .name_repair = "minimal")
}

records_to_tibble <- function(items, kind = NULL, level = "interaction") {
    if (identical(kind, "interaction") && level != "interaction") {
        if (!level %in% c("experiment", "observation")) {
            arg_error(paste("`level` must be \"interaction\", \"experiment\"",
                "or \"observation\"."))
        }
        rows <- unlist(lapply(items, interaction_rows, level = level),
            recursive = FALSE)
        return(rows_to_tibble(rows))
    }
    rows_to_tibble(lapply(items, record_row, kind = kind))
}
