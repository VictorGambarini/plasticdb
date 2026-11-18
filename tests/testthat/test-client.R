test_that("arguments beat options, which beat environment variables", {
    local_clean_env()
    withr::local_envvar(PLASTICDB_RELEASE = "2026.10",
        PLASTICDB_BASE_URL = "https://env.example/api/v1",
        PLASTICDB_API_KEY = "env-key")
    client <- plasticdb_client()
    expect_identical(client$release, "2026.10")
    expect_identical(client$base_url, "https://env.example/api/v1")
    expect_identical(client$api_key, "env-key")
    expect_identical(client$root_url, "https://env.example")

    withr::local_options(plasticdb.release = "2026.20")
    expect_identical(plasticdb_client()$release, "2026.20")
    expect_identical(plasticdb_client(release = "2026.30")$release, "2026.30")
    expect_identical(plasticdb_client(api_key = "arg")$api_key, "arg")
})

test_that("defaults: live database, no key, the public server", {
    local_clean_env()
    client <- plasticdb_client(cache = TRUE)
    expect_null(client$release)
    expect_null(client$api_key)
    expect_identical(client$base_url, "https://v2.plasticdb.org/api/v1")
    expect_identical(client$cache_dir, tools::R_user_dir("plasticdb", "cache"))
    expect_null(plasticdb_client(cache = FALSE)$cache_dir)
    expect_identical(plasticdb_client(cache = "/x/y")$cache_dir, "/x/y")
    expect_error(plasticdb_client(cache = 3), class = "plasticdb_argument_error")
    expect_identical(plasticdb_client(base_url = "https://h/api/v1/")$base_url,
        "https://h/api/v1")
})

test_that("blank values count as unset", {
    local_clean_env()
    withr::local_envvar(PLASTICDB_RELEASE = "  ", PLASTICDB_API_KEY = "")
    client <- plasticdb_client(release = "")
    expect_null(client$release)
    expect_null(client$api_key)
})

test_that("print never shows the API key", {
    local_clean_env()
    out <- capture.output(print(plasticdb_client(api_key = "secret-key")))
    expect_false(any(grepl("secret-key", out)))
    expect_true(any(grepl("api_key:  set", out, fixed = TRUE)))
})

test_that("requests carry the user agent and, with a key, a bearer token", {
    local_clean_env()
    seen <- new.env()
    httr2::local_mocked_responses(function(req) {
        seen$headers <- req$headers
        seen$ua <- req$options$useragent
        json_response(list(running = FALSE))
    })
    releases_status(client = plasticdb_client(api_key = "k1"))
    expect_match(seen$ua, "^plasticdb-r/[0-9.]+ \\(R/")
    auth <- seen$headers$Authorization
    if (inherits(auth, "weakref")) auth <- rlang::wref_value(auth)
    expect_identical(as.character(auth), "Bearer k1")
})

test_that("functions refuse something that is not a client", {
    expect_error(releases_status(client = list()),
        class = "plasticdb_argument_error")
})
