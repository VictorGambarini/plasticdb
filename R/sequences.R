#' Protein sequences as an AAStringSet
#'
#' `as_aastringset()` turns a table or record that carries sequences (a
#' protein or variant record, [proteins_variants()], [evidence_constructs()])
#' into a [Biostrings::AAStringSet]. Names are the accessions (`PlasticDB_<id>`
#' for the few records without one, as in the server's FASTA files); every
#' other column goes into `mcols()`. Records without a sequence are dropped.
#'
#' Protein list items carry no sequence, so [proteins_sequences()] and
#' [variants_sequences()] list the records and fetch the FASTA with the same
#' filters instead.
#'
#' @param x A tibble with a `sequence` column, or a record list from
#'   [proteins_get()] or [variants_get()].
#' @param ... Unused.
#'
#' @return An [Biostrings::AAStringSet] with metadata columns.
#' @export
#' @examples
#' variants <- tibble::tibble(id = 1:2, accession = c("PB0001", NA),
#'     variant_name = c("A", "B"), sequence = c("MKV", "MKL"))
#' as_aastringset(variants)
as_aastringset <- function(x, ...) {
    if (inherits(x, "plasticdb_record")) {
        fields <- unclass(x)
        attr(fields, "plasticdb") <- NULL
        if (is.null(fields$sequence)) {
            return(Biostrings::AAStringSet())
        }
        engineered <- !is.null(fields$variant_name) ||
            identical(fields$kind, "engineered")
        row <- rows_to_tibble(list(record_row(fields[c("id", "accession",
            intersect(c("name", "variant_name", "kind"), names(fields)))])))
        row$sequence <- fields$sequence
        x <- row
        x$.engineered <- engineered
    }
    if (!is.data.frame(x) || !"sequence" %in% names(x)) {
        arg_error(paste("`x` has no `sequence` column. Protein lists carry",
            "none: use proteins_sequences() instead."))
    }
    keep <- !is.na(x$sequence) & nzchar(x$sequence)
    x <- x[keep, , drop = FALSE]
    engineered <- if (".engineered" %in% names(x)) x$.engineered else
        "variant_name" %in% names(x) | (if ("kind" %in% names(x))
            x$kind %in% "engineered" else FALSE)
    x$.engineered <- NULL
    ids <- seq_ids(x, engineered)
    out <- Biostrings::AAStringSet(stats::setNames(x$sequence, ids))
    meta <- x[setdiff(names(x), "sequence")]
    S4Vectors::mcols(out) <- S4Vectors::DataFrame(meta, check.names = FALSE)
    out
}

seq_ids <- function(x, engineered) {
    acc <- if ("accession" %in% names(x)) as.character(x$accession) else
        rep(NA_character_, nrow(x))
    id <- if ("id" %in% names(x)) x$id else seq_len(nrow(x))
    engineered <- rep_len(engineered, nrow(x))
    fallback <- ifelse(engineered, paste0("PlasticDB_var_", id),
        paste0("PlasticDB_", id))
    ifelse(is.na(acc) | !nzchar(acc), fallback, acc)
}

# (seq_id, header, sequence) per FASTA record; the id ends at whitespace or |.
parse_fasta <- function(text) {
    lines <- trimws(strsplit(text, "\r?\n")[[1]])
    lines <- lines[nzchar(lines)]
    starts <- which(startsWith(lines, ">"))
    if (!length(starts)) {
        return(data.frame(seq_id = character(), header = character(),
            sequence = character(), stringsAsFactors = FALSE))
    }
    ends <- c(starts[-1] - 1L, length(lines))
    headers <- trimws(substring(lines[starts], 2))
    seqs <- mapply(function(s, e) {
        if (e > s) paste(lines[(s + 1L):e], collapse = "") else ""
    }, starts, ends)
    data.frame(seq_id = sub("[[:space:]|].*$", "", headers), header = headers,
        sequence = unname(seqs), stringsAsFactors = FALSE)
}

body_text <- function(data) {
    if (is.raw(data)) return(rawToChar(data))
    if (is.null(data)) "" else as.character(data)
}

# Listed proteins with their sequences from /downloads/proteins.fasta.
protein_sequences <- function(client, params, limit) {
    listed <- list_tibble(client, ENDPOINTS$proteins_list, params,
        limit = limit, kind = "protein")
    if (!nrow(listed)) {
        return(Biostrings::AAStringSet())
    }
    fasta_params <- if (is.null(client$release))
        params[intersect(names(params), PROTEIN_FILTERS)] else list()
    result <- pdb_request(client, ENDPOINTS$dl_proteins_fasta, fasta_params)
    fasta <- parse_fasta(body_text(result$data))
    ids <- seq_ids(listed, listed$kind %in% "engineered")
    found <- match(ids, fasta$seq_id)
    if (any(is.na(found))) {
        rlang::inform(sprintf("PlasticDB: %d of %d records have no sequence.",
            sum(is.na(found)), length(found)), class = "plasticdb_no_sequence")
    }
    keep <- !is.na(found)
    out <- Biostrings::AAStringSet(stats::setNames(fasta$sequence[found[keep]],
        ids[keep]))
    S4Vectors::mcols(out) <- S4Vectors::DataFrame(listed[keep, ],
        check.names = FALSE)
    S4Vectors::metadata(out)$plasticdb <- attr(listed, "plasticdb")
    out
}
