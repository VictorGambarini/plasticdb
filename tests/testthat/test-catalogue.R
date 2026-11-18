test_that("microorganisms", {
    local_clean_env()
    httptest2::with_mock_api({
        m <- microorganisms_list(genus = "Pseudomonas", limit = 2)
        one <- microorganisms_get(1547922)
        names_tbl <- microorganisms_names()
        levels <- microorganisms_lineage_levels()
        taxon <- microorganisms_taxonomy("genus", "Ideonella")
    })
    expect_identical(nrow(m), 2L)
    expect_true(all(grepl("^Pseudomonas", m$name)))
    expect_true(all(c("plastics_positive", "paper_ids", "paper_dois") %in%
        names(m)))
    expect_identical(one$tax_id, 1547922L)
    expect_true(all(c("id", "name") %in% names(names_tbl)))
    expect_true("Bacteria" %in% levels$domain)
    expect_identical(taxon$rank, "genus")
})

test_that("class_ is an alias of the rank filter class", {
    local_clean_env()
    calls <- local_api(function(req) json_response(page(list())))
    microorganisms_list(class_ = "Bacilli")
    expect_identical(query_of(calls$urls[[1]])$class, "Bacilli")
})

test_that("plastics", {
    local_clean_env()
    httptest2::with_mock_api({
        pl <- plastics_list(limit = 2)
        pet <- plastics_get(16, microorganism_limit = 2, protein_limit = 2)
        nm <- plastics_names()
    })
    expect_identical(nrow(pl), 2L)
    expect_type(pl$children, "list")
    expect_identical(pet$name, "PET")
    expect_gt(nrow(nm), 0)
})

test_that("papers", {
    local_clean_env()
    httptest2::with_mock_api({
        pp <- papers_list(limit = 2)
        one <- papers_get(1262)
        fp <- papers_footprint(1262)
        bib <- papers_export(1262)
    })
    expect_identical(nrow(pp), 2L)
    expect_identical(one$id, 1262L)
    expect_s3_class(fp, "plasticdb_record")
    expect_match(bib, "@")
    expect_error(papers_export(1262, year = 2020),
        class = "plasticdb_argument_error")
})

test_that("evidence material tables", {
    local_clean_env()
    httptest2::with_mock_api({
        b <- evidence_binding_assays(limit = 2)
        cs <- evidence_constructs(protein = "PBFCTW", limit = 2)
        c1 <- evidence_construct(397)
        st <- evidence_construct_structures(397)
        pr <- evidence_preparations(limit = 2)
        p1 <- evidence_preparation(127)
    })
    expect_s3_class(b, "tbl_df")
    expect_identical(nrow(cs), 2L)
    expect_identical(c1$id, 397L)
    expect_s3_class(st, "tbl_df")
    expect_identical(nrow(pr), 2L)
    expect_identical(p1$id, 127L)
    expect_s4_class(as_aastringset(cs), "AAStringSet")
})

test_that("search", {
    local_clean_env()
    httptest2::with_mock_api({
        hits <- search_hits("cutinase", limit = 3)
        grouped <- search_grouped("cutinase", limit = 1)
        auto <- search_autocomplete("cutin")
    })
    expect_identical(nrow(hits), 3L)
    expect_true(all(c("entity_type", "route_key", "rank") %in% names(hits)))
    expect_true("protein" %in% grouped$groups$group)
    expect_true(all(grouped$hits$group %in% grouped$groups$group))
    expect_type(auto$route_key, "character")
    expect_true("proteins" %in% auto$entity_type)
})

test_that("pathways", {
    local_clean_env()
    httptest2::with_mock_api({
        pl <- pathways_list()
        one <- pathways_get(1)
        forp <- pathways_for_plastic(16)
        g <- pathways_graph(16)
    })
    expect_true("step_count" %in% names(pl))
    expect_identical(one$id, 1L)
    expect_gt(nrow(forp), 0)
    expect_identical(names(g), c("nodes", "edges"))
    expect_identical(names(g$edges)[1:2], c("source", "target"))
    expect_true("id" %in% names(g$nodes))
})

test_that("stats", {
    local_clean_env()
    httptest2::with_mock_api({
        s <- stats_summary()
        a <- stats_protein_activity()
        ch <- stats_charts()
        bc <- stats_by_country()
    })
    expect_true(is.numeric(s$proteins))
    expect_s3_class(a, "plasticdb_record")
    expect_s3_class(ch[[1]], "tbl_df")
    expect_s3_class(bc$microorganisms, "tbl_df")
    expect_true(all(c("country", "count") %in% names(bc$microorganisms)))
})

test_that("the change feed carries since and as_of", {
    local_clean_env()
    httptest2::with_mock_api({
        feed <- changes(since = "2026-11-15T00:00:00Z", limit = 3)
    })
    expect_identical(nrow(feed), 3L)
    expect_true(all(feed$status %in% c("updated", "merged", "removed")))
    prov <- provenance(feed)
    expect_match(prov$as_of, "^2026-")
    expect_match(prov$since, "^2026-11-15")
    expect_error(changes(since = "2026-01-01", entity_type = "nope"))
})

test_that("dates and logicals are sent in the API's format", {
    local_clean_env()
    calls <- local_api(function(req) json_response(page(list())))
    proteins_list(updated_since = as.Date("2026-01-02"), has_structure = TRUE,
        min_kcat = 1e-4)
    q <- query_of(calls$urls[[1]])
    expect_identical(q$updated_since, "2026-01-02T00:00:00Z")
    expect_identical(q$has_structure, "true")
    expect_identical(q$min_kcat, "0.0001")
})

test_that("every route_key is a string", {
    local_clean_env()
    httptest2::with_mock_api({
        hits <- search_hits("cutinase", limit = 3)
        grouped <- search_grouped("cutinase", limit = 1)
    })
    expect_type(hits$route_key, "character")
    expect_type(grouped$hits$route_key, "character")
    expect_false(is.null(provenance(hits)))
    mixed <- tibble::tibble(route_key = list(1199L, "PBFCTW"))
    expect_identical(plasticdb:::route_keys_as_text(mixed)$route_key,
        c("1199", "PBFCTW"))
})

test_that("enzyme_only reaches the organism record and the stats", {
    local_clean_env()
    calls <- local_api(function(req) json_response(list(id = 1L)))
    microorganisms_get("PBCZU2", evidence_scope = "enzyme_only")
    stats_summary(evidence_scope = "enzyme_only")
    expect_identical(query_of(calls$urls[[1]])$evidence_scope, "enzyme_only")
    expect_identical(query_of(calls$urls[[2]])$evidence_scope, "enzyme_only")
    expect_error(stats_charts(evidence_scope = "enzyme_only"))
})

test_that("a merged organism names its survivor, or follows it", {
    local_clean_env()
    calls <- local_api(function(req) {
        if (grepl("PBOLD1", req$url)) {
            return(json_response(list(accession = "PBNEW1",
                location = "/api/v1/microorganisms/PBNEW1",
                kind = "microorganism", reason = "merged",
                merged_into = "PBNEW1"), status = 301L,
                headers = list(location = "/api/v1/microorganisms/PBNEW1")))
        }
        if (grepl("PBGONE", req$url)) {
            return(json_response(list(detail = "deleted",
                accession = "PBGONE", reason = "withdrawn"), status = 410L))
        }
        json_response(list(accession = "PBNEW1", name = "Survivor"))
    })
    err <- expect_error(microorganisms_get("PBOLD1"),
        class = "plasticdb_merged")
    expect_identical(err$kind, "microorganism")
    expect_identical(err$merged_into, "PBNEW1")
    expect_warning(m <- microorganisms_get("PBOLD1",
        evidence_scope = "enzyme_only", follow_merges = TRUE),
        class = "plasticdb_merged_warning")
    expect_identical(m$name, "Survivor")
    expect_identical(query_of(calls$urls[[3]])$evidence_scope, "enzyme_only")
    gone <- expect_error(microorganisms_get("PBGONE"), class = "plasticdb_gone")
    expect_identical(gone$accession, "PBGONE")
    expect_identical(gone$reason, "withdrawn")
})
