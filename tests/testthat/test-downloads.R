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
