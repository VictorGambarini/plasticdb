test_that("cite() names the live database before the first release", {
    local_clean_env()
    httptest2::with_mock_api({
        ref <- cite()
    })
    expect_s3_class(ref, "bibentry")
    expect_length(ref, 2)
    text <- format(ref, style = "text")
    expect_match(text[[1]], "live database")
    expect_match(text[[1]], "CC BY 4.0", fixed = TRUE)
    expect_match(text[[2]], "PlasticDB: a database of microorganisms")
    expect_identical(ref[[2]]$doi, "10.1093/database/baac008")
    expect_match(paste(utils::toBibtex(ref), collapse = "\n"), "@Article")
})

test_that("a pinned client cites its release and DOI", {
    local_clean_env()
    calls <- local_api(function(req) json_response(list(version = "2026.30",
        doi = "10.17608/k6.auckland.123", released_at = "2026-07-20T00:00:00Z",
        artefacts = NULL)))
    ref <- cite(plasticdb_client(release = "2026.30"))
    expect_match(calls$urls[[1]], "/releases/2026.30$")
    expect_identical(ref[[1]]$doi, "10.17608/k6.auckland.123")
    expect_identical(ref[[1]]$year, "2026")
    expect_match(format(ref[[1]], style = "text"), "release 2026.30")
    expect_match(paste(format(ref[[1]], style = "citation"), collapse = "\n"),
        "doi.org/10.17608")
})

test_that("an unreachable server still yields the paper, with a warning", {
    local_clean_env()
    local_no_sleep()
    local_api(function(req) json_response(list(detail = "down"),
        status = 502L))
    expect_warning(ref <- cite(plasticdb_client(release = "2026.30")),
        class = "plasticdb_citation_offline")
    expect_length(ref, 2)
    expect_match(format(ref[[1]], style = "text"), "2026.30")
})

test_that("an unknown pinned release is an error, not a citation", {
    local_clean_env()
    local_api(function(req) json_response(list(detail = "No release"),
        status = 404L))
    expect_error(cite(plasticdb_client(release = "2099.01")),
        class = "plasticdb_release_unknown")
})

test_that("server_version() reads /health", {
    local_clean_env()
    httptest2::with_mock_api({
        expect_match(server_version(), "^[0-9]+\\.[0-9]+")
    })
})
