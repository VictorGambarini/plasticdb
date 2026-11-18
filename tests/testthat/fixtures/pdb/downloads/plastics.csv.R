structure(list(method = "GET", url = "pdb/downloads/plastics.csv", 
    status_code = 200L, headers = structure(list(`content-disposition` = "attachment; filename=\"plasticdb_plastics.csv\"", 
        `content-type` = "text/csv; charset=utf-8", link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `x-ratelimit-limit` = "200", 
        `x-ratelimit-remaining` = "198"), class = "httr2_headers"), 
    body = structure(httptest2::find_mock_file("pdb/downloads/plastics.csv.R-FILE"), 
        class = "httr2_path"), timing = c(redirect = 0, namelookup = 3.9e-05, 
    connect = 3.9e-05, pretransfer = 0.000155, starttransfer = 0.020219, 
    total = 0.061518), cache = new.env(parent = emptyenv())), class = "httr2_response")
