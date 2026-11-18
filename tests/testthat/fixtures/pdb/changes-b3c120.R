structure(list(method = "GET", url = "pdb/changes?limit=3&offset=0&since=2026-11-15T00%3A00%3A00Z", 
    status_code = 200L, headers = structure(list(`cache-control` = "public, no-cache", 
        `content-type` = "application/json", etag = "W/\"none-2fe3a985f612a4182e62663a\"", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `x-ratelimit-limit` = "200", 
        `x-ratelimit-remaining` = "198", `content-length` = "563"), class = "httr2_headers"), 
    body = charToRaw("{\"since\":\"2026-11-15T00:00:00+00:00\",\"as_of\":\"2026-11-18T21:27:03.895394+00:00\",\"release\":null,\"total\":93,\"items\":[{\"entity_type\":\"microorganism\",\"id\":2813,\"accession\":\"PBU7FK\",\"status\":\"updated\",\"merged_into\":null,\"updated_at\":\"2026-11-15T03:26:14.124753+00:00\"},{\"entity_type\":\"microorganism\",\"id\":2884,\"accession\":\"PBERER\",\"status\":\"updated\",\"merged_into\":null,\"updated_at\":\"2026-11-15T03:26:46.818949+00:00\"},{\"entity_type\":\"microorganism\",\"id\":2885,\"accession\":\"PBF638\",\"status\":\"updated\",\"merged_into\":null,\"updated_at\":\"2026-11-15T03:26:57.442110+00:00\"}]}"), 
    timing = c(redirect = 0, namelookup = 3.9e-05, connect = 3.9e-05, 
    pretransfer = 0.000147, starttransfer = 0.066622, total = 0.066686
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
