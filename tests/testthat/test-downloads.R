test_that("a table download is saved under the server's file name", {
    local_clean_env()
    dir <- withr::local_tempdir()
    httptest2::with_mock_api({
        path <- downloads_plastics(dir, format = "csv")
    })
    expect_identical(basename(path), "plasticdb_plastics.csv")
    tbl <- utils::read.csv(path)
    expect_true(all(c("id", "name") %in% names(tbl)))
    expect_identical(last_response()$release_source, "live")
})

test_that("a file path is used as given, and filters are checked", {
    local_clean_env()
    dir <- withr::local_tempdir()
    calls <- local_api(function(req) httr2::response(status_code = 200L,
        headers = list(`content-type` = "text/plain",
            `x-plasticdb-release-source` = "live"),
        body = charToRaw(">PB0001 x\nMKV\n")))
    path <- downloads_proteins(file.path(dir, "pet.fasta"), format = "fasta",
        plastic = "PET")
    expect_identical(readLines(path), c(">PB0001 x", "MKV"))
    expect_match(calls$urls[[1]], "/downloads/proteins.fasta?plastic=PET",
        fixed = TRUE)
    expect_error(downloads_plastics(dir, nope = 1),
        class = "plasticdb_argument_error")
    expect_error(downloads_papers(dir, format = "fasta"))
})

release_record <- function(sha) {
    list(version = "2026.30", doi = "10.1/x", artefacts = list(
        manifest_sha256 = "0", files = list(list(path = "parquet/p.parquet",
            sha256 = sha, size = 3L))))
}

test_that("release artefacts are verified and cached", {
    local_clean_env()
    dir <- withr::local_tempdir()
    cache <- withr::local_tempdir()
    content <- charToRaw("abc")
    sha <- plasticdb:::sha256_bytes(content)
    calls <- local_api(function(req) {
        if (grepl("/artefacts/", req$url)) {
            return(httr2::response(status_code = 200L,
                headers = list(`content-type` = "application/octet-stream"),
                body = content))
        }
        json_response(release_record(sha))
    })
    client <- plasticdb_client(cache = cache)
    files <- downloads_artefacts("2026.30", client = client)
    expect_identical(files$path, "parquet/p.parquet")
    path <- downloads_release_file("parquet/p.parquet", dir,
        release = "2026.30", client = client)
    expect_identical(basename(path), "p.parquet")
    expect_identical(readBin(path, "raw", 10), content)
    n <- length(grep("/artefacts/", calls$urls))
    unlink(path)
    again <- downloads_release_file("parquet/p.parquet", dir,
        release = "2026.30", client = client)
    expect_identical(length(grep("/artefacts/", calls$urls)), n)
    expect_identical(readBin(again, "raw", 10), content)
})

test_that("a checksum mismatch keeps nothing", {
    local_clean_env()
    dir <- withr::local_tempdir()
    local_api(function(req) {
        if (grepl("/artefacts/", req$url)) {
            return(httr2::response(status_code = 200L,
                headers = list(`content-type` = "application/octet-stream"),
                body = charToRaw("tampered")))
        }
        json_response(release_record(strrep("a", 64)))
    })
    err <- expect_error(downloads_release_file("parquet/p.parquet",
        file.path(dir, "p.parquet"), release = "2026.30"),
        class = "plasticdb_checksum")
    expect_identical(err$expected, strrep("a", 64))
    expect_false(file.exists(file.path(dir, "p.parquet")))
})

test_that("a release without artefacts says so", {
    local_clean_env()
    local_api(function(req) json_response(list(version = "2026.30")))
    expect_error(downloads_artefacts("2026.30"),
        class = "plasticdb_release_no_artefacts")
})

test_that("artefacts need a release", {
    local_clean_env()
    httptest2::with_mock_api({
        expect_error(downloads_artefacts(), class = "plasticdb_release_error")
    })
})

test_that("the dataset ZIP waits while it is being built", {
    local_clean_env()
    sleeps <- local_no_sleep()
    dir <- withr::local_tempdir()
    n <- 0
    local_api(function(req) {
        n <<- n + 1
        if (n <= 4) {
            return(json_response(list(detail = "building"), status = 503L,
                headers = list(`retry-after` = "120")))
        }
        httr2::response(status_code = 200L, headers = list(
            `content-type` = "application/zip",
            `content-disposition` = "attachment; filename=\"plasticdb_dataset.zip\""),
            body = as.raw(c(0x50, 0x4b)))
    })
    expect_error(downloads_dataset(dir), class = "plasticdb_service_unavailable")
    n <- 0
    path <- downloads_dataset(dir, max_wait = 600)
    expect_identical(basename(path), "plasticdb_dataset.zip")
    expect_true(120 %in% sleeps$waits)
})

csv_response <- function(status = 200L, body = "id,name\n1,PET\n",
                            headers = list()) {
    httr2::response(status_code = status, headers = c(list(
        `content-type` = "text/csv",
        `x-plasticdb-release` = "2026.30",
        `x-plasticdb-release-source` = "snapshot",
        etag = "W/\"2026.30-abc\""), headers),
        body = if (status == 200L) charToRaw(body) else raw())
}

test_that("a cached pinned download is revalidated with its ETag", {
    local_clean_env()
    dir <- withr::local_tempdir()
    cache <- withr::local_tempdir()
    seen <- list()
    calls <- local_api(function(req) {
        seen[[length(seen) + 1L]] <<- req$headers
        if (length(seen) == 1L) {
            return(csv_response(headers = list(
                `x-plasticdb-source-sha256` = "aa, bb")))
        }
        csv_response(304L)
    })
    client <- plasticdb_client(release = "2026.30", cache = cache)
    first <- downloads_plastics(file.path(dir, "a.csv"), client = client)
    expect_identical(last_response()$etag, "W/\"2026.30-abc\"")
    expect_identical(last_response()$source_sha256, c("aa", "bb"))
    second <- downloads_plastics(file.path(dir, "b.csv"), client = client)
    expect_length(calls$urls, 2)
    expect_identical(seen[[2]][["If-None-Match"]], "W/\"2026.30-abc\"")
    expect_identical(readLines(second), readLines(first))
    expect_true(last_response()$from_cache)
})

test_that("an unreachable server falls back to the cached copy", {
    local_clean_env()
    local_no_sleep()
    dir <- withr::local_tempdir()
    cache <- withr::local_tempdir()
    n <- 0
    local_api(function(req) {
        n <<- n + 1
        if (n == 1) return(csv_response())
        stop(structure(class = c("httr2_failure", "error", "condition"),
            list(message = "Could not resolve host", call = NULL)))
    })
    client <- plasticdb_client(release = "2026.30", cache = cache,
        max_retries = 0)
    downloads_plastics(file.path(dir, "a.csv"), client = client)
    expect_warning(path <- downloads_plastics(file.path(dir, "b.csv"),
        client = client), class = "plasticdb_cache_fallback")
    expect_identical(readLines(path), c("id,name", "1,PET"))
})

test_that("an artefact is checked against the server's SHA-256 too", {
    local_clean_env()
    dir <- withr::local_tempdir()
    content <- charToRaw("abc")
    sha <- plasticdb:::sha256_bytes(content)
    local_api(function(req) {
        if (grepl("/artefacts/", req$url)) {
            return(httr2::response(status_code = 200L, headers = list(
                `content-type` = "application/octet-stream",
                `x-plasticdb-sha256` = strrep("f", 64)), body = content))
        }
        json_response(release_record(sha))
    })
    err <- expect_error(downloads_release_file("parquet/p.parquet", dir,
        release = "2026.30"), class = "plasticdb_checksum")
    expect_identical(err$expected, strrep("f", 64))
    expect_match(conditionMessage(err), "the server's")
})

test_that("the dataset ZIP's 503 is JSON with a reason", {
    local_clean_env()
    local_no_sleep()
    dir <- withr::local_tempdir()
    local_api(function(req) json_response(list(detail = "building",
        reason = "archive_unavailable"), status = 503L,
        headers = list(`retry-after` = "120")))
    err <- expect_error(downloads_dataset(dir),
        class = "plasticdb_service_unavailable")
    expect_identical(err$reason, "archive_unavailable")
    expect_identical(err$retry_after, 120)
})
