# Turning what a caller has (a path, FASTA text, a Biostrings set, a table)
# into job uploads.
#
# The server sniffs gzip by its magic bytes, but labels compare-genomes results
# after the file name and the HMM worker reads the `.gz` suffix, so every
# upload is named after its contents: `.gz` when (and only when) the bytes are
# gzip. Text and Biostrings sets are written to temporary files under those
# names; a caller's own file is sent as it is, under its content-derived name.

GZIP_MAGIC <- as.raw(c(0x1f, 0x8b))
# The extensions the server strips before `.gz` to name a genome (job_store).
INNER_EXTS <- c("fasta", "fa", "faa", "fna", "fas", "tsv", "csv", "txt", "tab")

is_gzip_file <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con))
    identical(readBin(con, "raw", 2L), GZIP_MAGIC)
}

upload_name <- function(stem, ext, gzipped) {
    paste0(stem, ".", ext, if (gzipped) ".gz" else "")
}

# A file name's stem without .gz and one known inner extension.
stem_of <- function(name) {
    if (tolower(tools::file_ext(name)) == "gz") {
        name <- sub("\\.[^.]*$", "", name)
        if (tolower(tools::file_ext(name)) %in% INNER_EXTS) {
            name <- sub("\\.[^.]*$", "", name)
        }
        return(name)
    }
    if (grepl(".", name, fixed = TRUE) && !startsWith(name, ".")) {
        name <- sub("\\.[^.]*$", "", name)
    }
    name
}

# The genome label the server derives from an upload's name
# (job_store.safe_stem).
safe_stem <- function(filename, index = 1L) {
    name <- basename(filename %||% "")
    if (!nzchar(name)) {
        return(sprintf("genome_%d", index))
    }
    stem <- gsub("[^A-Za-z0-9._-]", "_", stem_of(name))
    stem <- gsub("^[._-]+|[._-]+$", "", stem)
    if (nzchar(stem)) stem else sprintf("genome_%d", index)
}

is_xstringset <- function(x) {
    methods::is(x, "XStringSet")
}

is_xstring <- function(x) {
    methods::is(x, "XString")
}

looks_like_text <- function(value) {
    text <- trimws(value, "left")
    startsWith(text, ">") || grepl("\n", trimws(text), fixed = TRUE)
}

write_upload <- function(dir, name, writer) {
    path <- file.path(dir, name)
    writer(path)
    list(name = name, path = path)
}

from_path <- function(path, ext) {
    path <- path.expand(path)
    if (!file.exists(path) || dir.exists(path)) {
        arg_error(sprintf("No such file: %s", path))
    }
    if (!isTRUE(file.size(path) > 0)) {
        arg_error(sprintf("%s is empty.", path))
    }
    list(name = upload_name(stem_of(basename(path)), ext,
        is_gzip_file(path)), path = path)
}

# One upload from a path, text, raw bytes, a Biostrings set or (for a taxa
# table) a data frame. A string is a path when such a file exists, otherwise
# text (`>` FASTA, or several lines); a single line of residues becomes a
# one-record FASTA (`>query`). Temporary files go in `dir`.
to_upload <- function(value, ext, dir, stem = "input", kind = "FASTA") {
    if (is_xstringset(value) || is_xstring(value)) {
        return(upload_xstringset(value, ext, dir, stem, kind))
    }
    if (is.data.frame(value)) {
        return(upload_table(value, ext, dir, stem, kind))
    }
    if (is.raw(value)) {
        return(upload_raw(value, ext, dir, stem, kind))
    }
    upload_text(value, ext, dir, stem, kind)
}

upload_xstringset <- function(value, ext, dir, stem, kind) {
    if (ext != "fasta") {
        arg_error(sprintf("A %s cannot be the %s input.",
            class(value)[[1]], kind))
    }
    set <- value
    if (is_xstring(set)) {
        set <- methods::as(set, paste0(class(set)[[1]], "Set"))
    }
    if (!length(set)) {
        arg_error(sprintf("The %s input is empty.", kind))
    }
    if (is.null(names(set))) {
        names(set) <- sprintf("seq_%d", seq_along(set))
    }
    write_upload(dir, upload_name(stem, ext, FALSE),
        function(path) Biostrings::writeXStringSet(set, path))
}

upload_table <- function(value, ext, dir, stem, kind) {
    if (ext != "tsv") {
        arg_error(sprintf("A data frame cannot be the %s input.", kind))
    }
    if (!nrow(value)) {
        arg_error(sprintf("The %s input is empty.", kind))
    }
    clean_cells <- lapply(value, function(col) {
        col <- as.character(col)
        col[is.na(col)] <- ""
        gsub("[\t\r\n]", " ", col)
    })
    lines <- c(paste(gsub("[\t\r\n]", " ", names(value)), collapse = "\t"),
        do.call(paste, c(clean_cells, sep = "\t")))
    write_upload(dir, upload_name(stem, ext, FALSE),
        function(path) writeLines(lines, path, useBytes = TRUE))
}

upload_raw <- function(value, ext, dir, stem, kind) {
    if (!length(value)) {
        arg_error(sprintf("The %s input is empty.", kind))
    }
    gz <- length(value) >= 2L && identical(value[seq_len(2L)], GZIP_MAGIC)
    write_upload(dir, upload_name(stem, ext, gz),
        function(path) writeBin(value, path))
}

# FASTA text starts with '>'; a lone line of residues becomes `>query`.
fasta_text <- function(value) {
    text <- trimws(value)
    if (startsWith(text, ">")) {
        return(text)
    }
    single <- !grepl("\n", text, fixed = TRUE)
    if (single && grepl("^[A-Za-z*[:space:]-]+$", text) &&
        nchar(text) >= 5L) {
        text <- paste0(">query\n", text)
    } else if (single) {
        arg_error(sprintf(paste("%s is neither an existing file nor",
            "FASTA text (which starts with '>')."), encodeString(value,
            quote = "\"")))
    }
    text
}

upload_text <- function(value, ext, dir, stem, kind) {
    if (is.character(value) && length(value) > 1L) {
        value <- paste(value, collapse = "\n")
    }
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
        arg_error(sprintf(paste("Cannot use %s as the %s input: pass a file",
            "path, text, raw bytes or a Biostrings set."),
            class(value)[[1]], kind))
    }
    if (!looks_like_text(value) && nchar(value) < 4096L &&
        file.exists(path.expand(value)) && !dir.exists(path.expand(value))) {
        return(from_path(value, ext))
    }
    if (!nzchar(trimws(value))) {
        arg_error(sprintf("The %s input is empty.", kind))
    }
    # A table's trailing empty cells are data; only FASTA is trimmed.
    text <- if (ext == "fasta") fasta_text(value) else
        gsub("^[\r\n]+|[\r\n]+$", "", value)
    write_upload(dir, upload_name(stem, ext, FALSE),
        function(path) writeLines(text, path, useBytes = TRUE))
}

# The uploads of a genome comparison; each name is the genome's label in the
# results. `genomes` is a list (or character vector of paths) of genome
# inputs, named to label them; `labels` labels them instead.
genome_uploads <- function(genomes, labels, dir) {
    if (is_xstringset(genomes) || is.raw(genomes) ||
        (is.character(genomes) && length(genomes) == 1L)) {
        arg_error("compare_genomes() needs a list of at least two genomes.")
    }
    items <- as.list(genomes)
    keys <- names(genomes)
    if (!is.null(labels) && !is.null(keys) && any(nzchar(keys))) {
        arg_error("Label the genomes by their names or by `labels`, not both.")
    }
    if (is.null(labels) && !is.null(keys) && all(nzchar(keys))) {
        labels <- keys
    }
    if (length(items) < 2L) {
        arg_error("compare_genomes() needs at least two genomes to compare.")
    }
    if (!is.null(labels) && length(labels) != length(items)) {
        arg_error(sprintf("%d labels for %d genomes.", length(labels),
            length(items)))
    }
    uploads <- vector("list", length(items))
    seen <- character()
    for (i in seq_along(items)) {
        sub_dir <- file.path(dir, sprintf("g%d", i))
        dir.create(sub_dir, showWarnings = FALSE)
        upload <- to_upload(items[[i]], "fasta", sub_dir,
            stem = sprintf("genome_%d", i), kind = "genome FASTA")
        # A label is sanitised before the server's own stem rule applies,
        # so "E. coli K12" stays "E._coli_K12" rather than losing its tail.
        stem <- if (!is.null(labels)) safe_stem(paste0(gsub(
            "[^A-Za-z0-9._-]", "_", as.character(labels[[i]])), ".fasta"),
            i) else safe_stem(upload$name, i)
        if (stem %in% seen) {
            arg_error(sprintf(paste("Genomes %d and %d would both be",
                "labelled \"%s\"; labels name the results, so they must be",
                "distinct (pass `labels`)."), match(stem, seen), i, stem))
        }
        seen <- c(seen, stem)
        upload$name <- upload_name(stem, "fasta", is_gzip_file(upload$path))
        uploads[[i]] <- upload
    }
    uploads
}

# A single query sequence (residues only) from a string, one-record FASTA
# text or a Biostrings sequence.
sequence_text <- function(value) {
    if (is_xstringset(value)) {
        if (length(value) != 1L) {
            arg_error(paste("search_sequence() takes one sequence; submit",
                "several with annotate_gene()."))
        }
        return(as.character(value[[1]]))
    }
    if (is_xstring(value)) {
        return(as.character(value))
    }
    if (!is.character(value) || length(value) < 1L || anyNA(value)) {
        arg_error("`sequence` must be a string or a Biostrings sequence.")
    }
    text <- trimws(paste(value, collapse = "\n"))
    if (startsWith(text, ">")) {
        records <- strsplit(text, ">", fixed = TRUE)[[1]]
        records <- records[nzchar(trimws(records))]
        if (length(records) != 1L) {
            arg_error(paste("search_sequence() takes one sequence; submit",
                "several with annotate_gene()."))
        }
        lines <- strsplit(records[[1]], "\n", fixed = TRUE)[[1]][-1]
        text <- paste(trimws(lines), collapse = "")
    }
    text <- gsub("[[:space:]]+", "", text)
    if (!nzchar(text)) {
        arg_error("The sequence is empty.")
    }
    text
}
