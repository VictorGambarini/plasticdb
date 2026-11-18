test_that("a pinned client sends ?release= and caches snapshot answers", {
    local_clean_env()
    cache <- withr::local_tempdir()
    client <- plasticdb_client(release = "2026.30", cache = cache)
    calls <- local_api(function(req) {
        json_response(page(list(list(id = 1L, accession = "PB0001",
            name = "PETase"))), release = "2026.30", source = "snapshot")
    })
    first <- proteins_list(plastic = "PET", client = client)
    expect_identical(query_of(calls$urls[[1]])$release, "2026.30")
    expect_true(provenance(first)$pinned)
    expect_identical(provenance(first)$release, "2026.30")
    expect_false(provenance(first)$from_cache)

    second <- proteins_list(plastic = "PET", client = client)
    expect_length(calls$urls, 1)
    expect_true(provenance(second)$from_cache)
    expect_identical(second$accession, "PB0001")
    expect_identical(last_response()$release_source, "snapshot")
})

test_that("live answers are never cached, even for a pinned client", {
    local_clean_env()
    cache <- withr::local_tempdir()
    client <- plasticdb_client(release = "2026.50", cache = cache)
    calls <- local_api(function(req) {
        json_response(page(list(list(id = 1L))), release = "2026.50",
            source = "live")
    })
    proteins_list(client = client)
    proteins_list(client = client)
    expect_length(calls$urls, 2)
})

test_that("an unpinned client never touches the cache", {
    local_clean_env()
    cache <- file.path(withr::local_tempdir(), "never")
    client <- plasticdb_client(cache = cache)
    calls <- local_api(function(req) json_response(page(list()),
        source = "snapshot", release = "2026.30"))
    proteins_list(client = client)
    proteins_list(client = client)
    expect_length(calls$urls, 2)
    expect_false(dir.exists(cache))
    expect_null(query_of(calls$urls[[1]])$release)
})

test_that("a snapshot of another release is not cached", {
    local_clean_env()
    cache <- withr::local_tempdir()
    client <- plasticdb_client(release = "2026.30", cache = cache)
    calls <- local_api(function(req) json_response(page(list()),
        source = "snapshot", release = "2026.20"))
    proteins_list(client = client)
    proteins_list(client = client)
    expect_length(calls$urls, 2)
})

test_that("release metadata is never pinned", {
    local_clean_env()
    calls <- local_api(function(req) json_response(list(running = FALSE)))
    releases_status(client = plasticdb_client(release = "2026.30"))
    expect_null(query_of(calls$urls[[1]])$release)
})

test_that("a pinned live-only call gets the live_only error", {
    local_clean_env()
    calls <- local_api(function(req) json_response(list(
        detail = "Search is live only", release = "2026.30",
        reason = "live_only"), status = 409L))
    expect_error(search_hits("PET",
        client = plasticdb_client(release = "2026.30")),
        class = "plasticdb_release_live_only")
    expect_identical(query_of(calls$urls[[1]])$release, "2026.30")
})

test_that("the unavailable-fields header is reported", {
    local_clean_env()
    local_api(function(req) json_response(page(list(list(id = 1L))),
        release = "2026.30", source = "snapshot",
        headers = list(`x-plasticdb-snapshot-unavailable` = "origin, gene")))
    out <- proteins_list(client = plasticdb_client(release = "2026.30"))
    expect_identical(provenance(out)$unavailable_fields, c("origin", "gene"))
})

test_that("provenance() of the client names the release and versions", {
    local_clean_env()
    local_api(function(req) {
        if (grepl("/health", req$url)) {
            return(json_response(list(status = "ok", version = "2.87.0")))
        }
        json_response(page(list(list(id = 1L))), release = "2026.30",
            source = "snapshot")
    })
    client <- plasticdb_client(release = "2026.30")
    proteins_list(client = client)
    prov <- provenance(client = client)
    expect_identical(prov$release, "2026.30")
    expect_identical(prov$release_source, "snapshot")
    expect_true(prov$pinned)
    expect_identical(prov$data_license, "CC BY 4.0")
    expect_identical(prov$sdk_version,
        as.character(utils::packageVersion("plasticdb")))
    expect_identical(prov$server_version, "2.87.0")
    expect_error(provenance(list()), class = "plasticdb_argument_error")
})

test_that("an unpinned client says once that it is unpinned", {
    local_clean_env()
    withr::local_options(plasticdb.quiet = FALSE)
    state <- plasticdb:::.state
    old <- state$announced
    state$announced <- FALSE
    withr::defer(state$announced <- old)
    local_api(function(req) json_response(page(list()), release = "2026.50"))
    expect_message(proteins_list(), class = "plasticdb_unpinned")
    expect_no_message(proteins_list())
})
