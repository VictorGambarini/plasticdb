test_that("proteins_list() returns flat rows with provenance", {
    local_clean_env()
    httptest2::with_mock_api({
        pet <- proteins_list(plastic = "PET", kind = "wild_type", limit = 3)
    })
    expect_s3_class(pet, "tbl_df")
    expect_identical(nrow(pet), 3L)
    expect_true(all(c("accession", "name", "source_microorganism.name",
        "plastics_positive", "plastics_negative", "paper_ids") %in%
        names(pet)))
    expect_false("degradation_links" %in% names(pet))
    expect_false("source_microorganism.degradation_links" %in% names(pet))
    expect_type(pet$plastics_positive, "list")
    expect_true(all(vapply(pet$plastics_positive,
        function(x) "PET" %in% x, TRUE)))
    prov <- provenance(pet)
    expect_identical(prov$release_source, "live")
    expect_false(prov$pinned)
    expect_gt(prov$total, 3)
    expect_match(prov$url, "plastic=PET")
})

test_that("an unknown filter is refused before anything is sent", {
    local_clean_env()
    calls <- local_api(function(req) stop("no request expected"))
    expect_error(proteins_list(nope = 1), class = "plasticdb_argument_error")
    expect_error(proteins_list("PET"), class = "plasticdb_argument_error")
    expect_length(calls$urls, 0)
})

test_that("a bad filter value is a server validation error", {
    local_clean_env()
    httptest2::with_mock_api({
        err <- expect_error(proteins_list(sort_by = "nope", limit = 1),
            class = "plasticdb_validation")
    })
    expect_identical(err$status, 422L)
    expect_match(conditionMessage(err), "sort_by")
    expect_length(err$errors, 1)
})

test_that("proteins_get() returns the record", {
    local_clean_env()
    httptest2::with_mock_api({
        p <- proteins_get("PBFCTW", view = "summary")
    })
    expect_s3_class(p, "plasticdb_record")
    expect_identical(p$accession, "PBFCTW")
    expect_identical(p$name, "PETase")
    expect_match(p$sequence, "^MNFPRASRLMQ")
    expect_output(print(p), "plasticdb_record")
    seqs <- as_aastringset(p)
    expect_s4_class(seqs, "AAStringSet")
    expect_identical(names(seqs), "PBFCTW")
})

test_that("a merged accession names its survivor, or follows it", {
    local_clean_env()
    httptest2::with_mock_api({
        err <- expect_error(proteins_get("PBGQPC"), class = "plasticdb_merged")
        expect_s3_class(err, "plasticdb_gone")
        expect_identical(err$merged_into, "PBMW9H")
        expect_identical(err$location, "/api/v1/proteins/PBMW9H")
        expect_warning(p <- proteins_get("PBGQPC", follow_merges = TRUE),
            class = "plasticdb_merged_warning")
    })
    expect_identical(p$accession, "PBMW9H")
})

test_that("a variant's accession returns the variant", {
    local_clean_env()
    httptest2::with_mock_api({
        v <- proteins_get("PBDTZX")
        err <- expect_error(proteins_interactions("PBDTZX", limit = 3),
            class = "plasticdb_engineered_variant")
    })
    expect_identical(v$accession, "PBDTZX")
    expect_false(is.null(v$variant_name))
    expect_identical(err$accession, "PBDTZX")
    expect_identical(err$location, "/api/v1/variants/PBDTZX")
})

test_that("interactions come at three levels", {
    local_clean_env()
    httptest2::with_mock_api({
        i <- proteins_interactions("PBFCTW", limit = 3)
        e <- proteins_interactions("PBFCTW", level = "experiment", limit = 3)
        o <- evidence_interactions("PBFCTW", level = "observation", limit = 3)
    })
    expect_identical(nrow(i), 3L)
    expect_true(all(c("methods", "experiment_count", "observation_count",
        "plastic.name") %in% names(i)))
    expect_false("experiments" %in% names(i))
    expect_identical(nrow(e), as.integer(sum(pmax(i$experiment_count, 1))))
    expect_true("experiment.method" %in% names(e))
    expect_true(any(startsWith(names(o), "observation.")))
    expect_gte(nrow(o), nrow(e))
})

test_that("kinetics, properties and variants are tables", {
    local_clean_env()
    httptest2::with_mock_api({
        k <- proteins_kinetics("PBFCTW", limit = 2)
        p <- evidence_properties("PBFCTW", limit = 2)
        v <- proteins_variants("PBFCTW", limit = 2)
    })
    expect_identical(nrow(k), 2L)
    expect_identical(nrow(p), 2L)
    expect_identical(nrow(v), 2L)
    expect_true(all(c("mutations", "kinetics_count", "sequence") %in%
        names(v)))
    seqs <- as_aastringset(v)
    expect_s4_class(seqs, "AAStringSet")
    expect_identical(names(seqs), v$accession)
    expect_identical(S4Vectors::mcols(seqs)$variant_name, v$variant_name)
})

test_that("proteins_sequences() pairs the list with the FASTA download", {
    local_clean_env()
    httptest2::with_mock_api({
        s <- proteins_sequences(plastic = "PHB", kind = "wild_type",
            min_year = 2025)
    })
    expect_s4_class(s, "AAStringSet")
    expect_gt(length(s), 0)
    expect_true(all(grepl("^PB", names(s))))
    expect_identical(names(s), S4Vectors::mcols(s)$accession)
    expect_identical(provenance(s)$release_source, "live")
})

test_that("FASTA text and engineered parents", {
    local_clean_env()
    httptest2::with_mock_api({
        fa <- proteins_fasta("PBFCTW")
        parents <- proteins_engineered_parents(limit = 2)
        vfa <- variants_fasta("PBDTZX")
    })
    expect_match(fa, "^>PBFCTW")
    expect_match(vfa, "^>PBDTZX")
    expect_identical(nrow(parents), 2L)
})

test_that("variants: list, get, merges and withdrawals", {
    local_clean_env()
    httptest2::with_mock_api({
        vl <- variants_list(plastic = "PET", limit = 2)
        expect_error(variants_get("PBG3V3"), class = "plasticdb_merged")
        expect_warning(v <- variants_get("PBG3V3", follow_merges = TRUE),
            class = "plasticdb_merged_warning")
        expect_error(variants_get("PB8B8P"), class = "plasticdb_not_found")
        vo <- variants_of("PBFCTW", limit = 2)
    })
    expect_true(all(vl$kind == "engineered"))
    expect_match(provenance(vl)$url, "kind=engineered")
    expect_identical(v$accession, "PBTNBA")
    expect_identical(nrow(vo), 2L)
    expect_error(variants_list(kind = "wild_type"),
        class = "plasticdb_argument_error")
})
