interaction_item <- function(id = 1L, ...) {
    list(id = id, actor_type = "protein", protein_accession = "PB0001",
        microorganism_accession = NULL, plastic_id = 16L, paper_id = 3L,
        polarity = "positive", product_compound_ids = list(4L, 5L), ...)
}

test_that("the top-level evidence lists are tibbles with paging and filters", {
    local_clean_env()
    calls <- local_api(function(req) {
        json_response(list(total = 2L, offset = 0L, limit = 500L,
            items = list(interaction_item(1L), interaction_item(2L))))
    })
    x <- interactions_list(plastic = "PET", include_subtypes = TRUE,
        evidence_level = "direct", protein = "PB0001", has_quantitative = TRUE)
    expect_s3_class(x, "tbl_df")
    expect_identical(nrow(x), 2L)
    expect_identical(provenance(x)$total, 2)
    expect_identical(x$protein_accession, c("PB0001", "PB0001"))
    expect_identical(x$product_compound_ids[[1]], c(4L, 5L))
    expect_match(calls$urls[[1]], "/api/v1/interactions\\?")
    q <- query_of(calls$urls[[1]])
    expect_identical(q$evidence_level, "direct")
    expect_identical(q$protein, "PB0001")
    expect_identical(q$plastic, "PET")
    expect_identical(q$limit, "500")
    expect_identical(q$offset, "0")
})

test_that("experiments and observations take their parents' ids", {
    local_clean_env()
    calls <- local_api(function(req) json_response(page(list(list(id = 9L)))))
    experiments_list(interaction_id = 4L, evidence_method = "growth")
    observations_list(interaction_id = 4L, experiment_id = 8L,
        measurement_type = "weight_loss")
    expect_match(calls$urls[[1]], "/experiments\\?")
    expect_identical(query_of(calls$urls[[1]])$interaction_id, "4")
    expect_match(calls$urls[[2]], "/observations\\?")
    expect_identical(query_of(calls$urls[[2]])$experiment_id, "8")
    expect_error(experiments_list(experiment_id = 1),
        class = "plasticdb_argument_error")
    expect_error(interactions_list(interaction_id = 1),
        class = "plasticdb_argument_error")
    expect_error(observations_list(nope = 1),
        class = "plasticdb_argument_error")
})

test_that("evidence records are fetched by id", {
    local_clean_env()
    calls <- local_api(function(req) json_response(list(id = 7L)))
    expect_identical(interactions_get(7)$id, 7L)
    expect_s3_class(experiments_get(7), "plasticdb_record")
    observations_get(7)
    expect_length(calls$urls, 3)
    expect_match(calls$urls[[1]], "/interactions/7$")
    expect_match(calls$urls[[2]], "/experiments/7$")
    expect_match(calls$urls[[3]], "/observations/7$")
})

test_that("a 404 evidence id is plasticdb_not_found", {
    local_clean_env()
    local_api(function(req) json_response(list(detail = "Not found"), 404L))
    expect_error(interactions_get(1), class = "plasticdb_not_found")
})

test_that("the evidence lists are pinnable", {
    local_clean_env()
    client <- plasticdb_client(release = "2026.30")
    calls <- local_api(function(req) json_response(page(list(list(id = 1L))),
        release = "2026.30", source = "snapshot"))
    x <- observations_list(client = client)
    expect_identical(query_of(calls$urls[[1]])$release, "2026.30")
    expect_true(provenance(x)$pinned)
})

test_that("proteins_get() asks for the summary view unless told otherwise", {
    local_clean_env()
    calls <- local_api(function(req) json_response(list(id = 1L,
        accession = "PB0001", counts = list(interactions = 3L))))
    p <- proteins_get("PB0001")
    expect_identical(query_of(calls$urls[[1]])$view, "summary")
    expect_identical(p$counts$interactions, 3L)
    proteins_get("PB0001", view = "full")
    expect_identical(query_of(calls$urls[[2]])$view, "full")
    expect_error(proteins_get("PB0001", view = "nope"))
})

test_that("kind is a deprecated alias of evidence_level, never sent with it", {
    local_clean_env()
    calls <- local_api(function(req) json_response(page(list())))
    proteins_interactions("PB0001", "direct")
    q <- query_of(calls$urls[[1]])
    expect_identical(q$evidence_level, "direct")
    expect_null(q$kind)
    proteins_interactions("PB0001")
    expect_identical(query_of(calls$urls[[2]])$evidence_level, "all")

    rlang::reset_warning_verbosity("plasticdb_kind_proteins_interactions")
    expect_warning(proteins_interactions("PB0001", kind = "indirect"),
        class = "plasticdb_deprecated")
    q <- query_of(calls$urls[[3]])
    expect_identical(q$evidence_level, "indirect")
    expect_null(q$kind)

    suppressWarnings(proteins_interactions("PB0001", kind = "direct",
        evidence_level = "direct"))
    expect_identical(query_of(calls$urls[[4]])$evidence_level, "direct")
    expect_error(suppressWarnings(proteins_interactions("PB0001",
        kind = "direct", evidence_level = "indirect")),
        class = "plasticdb_argument_error")
    expect_length(calls$urls, 4)

    suppressWarnings(evidence_interactions("PB0001", kind = "indirect"))
    expect_identical(query_of(calls$urls[[5]])$evidence_level, "indirect")
    evidence_interactions("PB0001", evidence_level = "direct")
    q <- query_of(calls$urls[[6]])
    expect_identical(q$evidence_level, "direct")
    expect_null(q$kind)
})

seq_item <- function(id, acc, seq, ...) {
    list(id = id, accession = acc, name = "PETase", kind = "wild_type",
        sequence = seq, ...)
}

test_that("proteins_sequences() uses the list's own sequences", {
    local_clean_env()
    calls <- local_api(function(req) {
        json_response(page(list(seq_item(1L, "PB0001", "MKV"),
            seq_item(2L, "PB0002", "MKL"), seq_item(3L, "PB0003", NULL))))
    })
    expect_message(s <- proteins_sequences(plastic = "PET"),
        "1 of 3 records have no sequence", class = "plasticdb_no_sequence")
    expect_length(calls$urls, 1)
    expect_identical(query_of(calls$urls[[1]])$include_sequence, "true")
    expect_identical(names(s), c("PB0001", "PB0002"))
    expect_identical(as.character(s[[2]]), "MKL")
    expect_false("sequence" %in% names(S4Vectors::mcols(s)))
    expect_identical(S4Vectors::mcols(s)$accession, c("PB0001", "PB0002"))
})

test_that("pinned sequences need no FASTA download either", {
    local_clean_env()
    client <- plasticdb_client(release = "2026.30")
    calls <- local_api(function(req) json_response(page(list(
        seq_item(1L, "PB0001", "MKV"))), release = "2026.30",
        source = "snapshot"))
    s <- variants_sequences(client = client)
    expect_length(calls$urls, 1)
    q <- query_of(calls$urls[[1]])
    expect_identical(q$release, "2026.30")
    expect_identical(q$kind, "engineered")
    expect_identical(as.character(s[[1]]), "MKV")
})

test_that("a server without include_sequence falls back to the FASTA", {
    local_clean_env()
    calls <- local_api(function(req) {
        if (grepl("downloads/proteins.fasta", req$url, fixed = TRUE)) {
            httr2::response(status_code = 200L, url = req$url,
                headers = list(`content-type` = "text/plain"),
                body = charToRaw(">PB0001 PETase\nMKV\n>PB0002 x\nMKL\n"))
        } else {
            json_response(page(list(list(id = 1L, accession = "PB0001",
                kind = "wild_type"))))
        }
    })
    s <- proteins_sequences(plastic = "PET")
    expect_length(calls$urls, 2)
    expect_identical(as.character(s[[1]]), "MKV")
})

test_that("proteins_list(include_sequence = TRUE) adds the sequence column", {
    local_clean_env()
    calls <- local_api(function(req) json_response(page(list(
        seq_item(1L, "PB0001", "MKV")))))
    x <- proteins_list(include_sequence = TRUE)
    expect_identical(x$sequence, "MKV")
    expect_identical(query_of(calls$urls[[1]])$include_sequence, "true")
    expect_s4_class(as_aastringset(x), "AAStringSet")
})

test_that("proteins_fasta() is pinnable (server 2.87.1)", {
    local_clean_env()
    client <- plasticdb_client(release = "2026.30")
    calls <- local_api(function(req) httr2::response(status_code = 200L,
        url = req$url, headers = list(`content-type` = "text/plain",
            `x-plasticdb-release` = "2026.30",
            `x-plasticdb-release-source` = "snapshot"),
        body = charToRaw(">PB0001\nMKV\n")))
    expect_match(proteins_fasta("PB0001", client = client), "^>PB0001")
    expect_identical(query_of(calls$urls[[1]])$release, "2026.30")
})
