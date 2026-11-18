expect_classes <- function(status, body, classes, headers = list()) {
    local_api(function(req) json_response(body, status = status,
        headers = headers))
    err <- tryCatch(releases_status(), error = function(e) e)
    for (cls in c(classes, "plasticdb_error")) {
        expect_s3_class(err, cls)
    }
    invisible(err)
}

test_that("HTTP statuses map onto the condition classes", {
    local_clean_env()
    local_no_sleep()
    expect_classes(400L, list(detail = "bad"), "plasticdb_bad_request")
    expect_classes(401L, list(detail = "no key"), "plasticdb_authentication")
    expect_classes(403L, list(detail = "no"), "plasticdb_permission_denied")
    expect_classes(404L, list(detail = "missing"), "plasticdb_not_found")
    expect_classes(409L, list(detail = "conflict"), "plasticdb_conflict")
    expect_classes(410L, list(detail = "withdrawn"), "plasticdb_gone")
    expect_classes(413L, list(detail = "big"), "plasticdb_payload_too_large")
    err <- expect_classes(404L, list(detail = "missing"), "plasticdb_not_found")
    expect_identical(conditionMessage(err), "404 Not Found: missing")
    expect_identical(err$detail, "missing")
})

test_that("release errors carry the release and reason", {
    local_clean_env()
    local_no_sleep()
    err <- expect_classes(404L, list(detail = "No release 2026.99",
        release = "2026.99", reason = "unknown"),
        c("plasticdb_release_unknown", "plasticdb_release_not_available",
            "plasticdb_release_error", "plasticdb_not_found"))
    expect_identical(err$release, "2026.99")
    expect_identical(err$reason, "unknown")
    expect_classes(404L, list(detail = "x", reason = "no_artefacts"),
        c("plasticdb_release_no_artefacts", "plasticdb_not_found"))
    err <- expect_classes(409L, list(detail = "live only", release = "2026.30",
        current_release = "2026.50", reason = "live_only"),
        c("plasticdb_release_live_only", "plasticdb_release_unsupported",
            "plasticdb_conflict"))
    expect_identical(err$current_release, "2026.50")
    expect_classes(503L, list(detail = "not mounted", reason = "not_mounted"),
        c("plasticdb_release_not_mounted", "plasticdb_service_unavailable",
            "plasticdb_server_error"))
    err <- expect_classes(422L, list(detail = list(list(
        type = "release_unsupported", loc = list("query", "plastic"),
        msg = "not in this release"))),
        c("plasticdb_release_unsupported_filter", "plasticdb_validation"))
    expect_identical(err$parameters, "plastic")
})

test_that("an unknown release from releases_get() is a release error", {
    local_clean_env()
    httptest2::with_mock_api({
        err <- expect_error(releases_get("2026.99"),
            class = "plasticdb_release_unknown")
    })
    expect_s3_class(err, "plasticdb_not_found")
    expect_identical(err$release, "2026.99")
})

test_that("429 waits for Retry-After, then succeeds", {
    local_clean_env()
    sleeps <- local_no_sleep()
    n <- 0
    calls <- local_api(function(req) {
        n <<- n + 1
        if (n == 1) {
            json_response(list(detail = "slow down"), status = 429L,
                headers = list(`retry-after` = "2"))
        } else {
            json_response(list(running = FALSE))
        }
    })
    out <- releases_status()
    expect_false(out$running)
    expect_identical(sleeps$waits, 2)
    expect_length(calls$urls, 2)
})

test_that("429 beyond max_retry_wait raises at once with retry_after", {
    local_clean_env()
    sleeps <- local_no_sleep()
    local_api(function(req) json_response(list(detail = "slow"),
        status = 429L, headers = list(`retry-after` = "3600")))
    err <- expect_error(releases_status(), class = "plasticdb_rate_limit")
    expect_identical(err$retry_after, 3600)
    expect_length(sleeps$waits, 0)
})

test_that("5xx is retried with backoff, then raised", {
    local_clean_env()
    sleeps <- local_no_sleep()
    calls <- local_api(function(req) json_response(list(detail = "boom"),
        status = 502L))
    expect_error(releases_status(), class = "plasticdb_server_error")
    expect_length(calls$urls, 4)
    expect_length(sleeps$waits, 3)
    expect_true(all(diff(sleeps$waits) > 0))
})

test_that("a 503 not_mounted is not retried", {
    local_clean_env()
    sleeps <- local_no_sleep()
    calls <- local_api(function(req) json_response(list(detail = "x",
        reason = "not_mounted"), status = 503L))
    expect_error(releases_status(), class = "plasticdb_release_not_mounted")
    expect_length(calls$urls, 1)
})

test_that("max_retries = 0 sends once", {
    local_clean_env()
    local_no_sleep()
    calls <- local_api(function(req) json_response(list(), status = 500L))
    expect_error(releases_status(client = plasticdb_client(max_retries = 0)),
        class = "plasticdb_server_error")
    expect_length(calls$urls, 1)
})

test_that("Retry-After takes seconds or an HTTP date", {
    expect_identical(plasticdb:::parse_retry_after("7"), 7)
    expect_null(plasticdb:::parse_retry_after(NULL))
    expect_null(plasticdb:::parse_retry_after("soon"))
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
        "Oct", "Nov", "Dec")
    t <- as.POSIXlt(Sys.time() + 30, tz = "GMT")
    later <- sprintf("%s, %02d %s %d %02d:%02d:%02d GMT",
        c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[t$wday + 1],
        t$mday, months[t$mon + 1], t$year + 1900, t$hour, t$min,
        floor(t$sec))
    wait <- plasticdb:::parse_retry_after(later)
    expect_gt(wait, 20)
    expect_lte(wait, 31)
})

test_that("an unexpected redirect is reported as such", {
    local_clean_env()
    local_api(function(req) json_response(list(), status = 302L,
        headers = list(location = "/elsewhere")))
    err <- expect_error(releases_status(),
        class = "plasticdb_unexpected_response")
    expect_match(conditionMessage(err), "/elsewhere")
})
