paged_api <- function(total, release = function(offset) NULL) {
    local_api(function(req) {
        q <- query_of(req$url)
        offset <- as.integer(q$offset)
        size <- as.integer(q$limit)
        ids <- seq_len(total)
        ids <- ids[ids > offset]
        ids <- utils::head(ids, size)
        json_response(page(lapply(ids, function(i) list(id = i)), total),
            release = release(offset))
    }, env = parent.frame())
}

test_that("lists fetch every page at 500 a time", {
    local_clean_env()
    calls <- paged_api(1203)
    out <- plastics_list()
    expect_identical(nrow(out), 1203L)
    expect_identical(out$id, seq_len(1203))
    expect_length(calls$urls, 3)
    expect_identical(query_of(calls$urls[[1]])$limit, "500")
    expect_identical(query_of(calls$urls[[3]])$offset, "1000")
    expect_identical(provenance(out)$total, 1203)
})

test_that("limit caps the records and the page size", {
    local_clean_env()
    calls <- paged_api(1203)
    out <- plastics_list(limit = 7)
    expect_identical(nrow(out), 7L)
    expect_length(calls$urls, 1)
    expect_identical(query_of(calls$urls[[1]])$limit, "7")
    expect_error(plastics_list(limit = -1), class = "plasticdb_argument_error")
})

test_that("limit = 0 returns an empty table with the total", {
    local_clean_env()
    paged_api(12)
    out <- plastics_list(limit = 0)
    expect_identical(nrow(out), 0L)
    expect_identical(provenance(out)$total, 12)
})

test_that("a release change while paging warns", {
    local_clean_env()
    paged_api(600, release = function(offset) if (offset == 0) "2026.49"
        else "2026.50")
    expect_warning(plastics_list(), class = "plasticdb_release_changed")
})

test_that("a non-envelope answer is an unexpected response", {
    local_clean_env()
    local_api(function(req) json_response(list(1, 2)))
    expect_error(plastics_list(), class = "plasticdb_unexpected_response")
})

test_that("rows flatten nested objects and keep lists as list columns", {
    rows <- plasticdb:::records_to_tibble(list(
        list(id = 1L, org = list(name = "A", tax = 1L), tags = list("x", "y"),
            v = NULL),
        list(id = 2L, org = NULL, tags = list(), v = 2.5)))
    expect_identical(names(rows), c("id", "org.name", "org.tax", "tags", "v"))
    expect_identical(rows$org.name, c("A", NA))
    expect_identical(rows$tags, list(c("x", "y"), list()))
    expect_identical(rows$v, c(NA, 2.5))
})
