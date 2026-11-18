structure(list(method = "GET", url = "pdb/stats/protein-activity", 
    status_code = 200L, headers = structure(list(`cache-control` = "public, no-cache", 
        `content-type` = "application/json", etag = "W/\"none-b42838e9c75ec812ec65ac53\"", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `content-length` = "367"), class = "httr2_headers"), 
    body = charToRaw("{\"wild_type\":{\"degrader\":853,\"negative_only\":244,\"inconclusive_only\":28,\"mixed_nonpositive\":5,\"untested\":35,\"tested\":1130},\"engineered\":{\"degrader\":2982,\"negative_only\":248,\"inconclusive_only\":64,\"mixed_nonpositive\":1,\"untested\":101,\"tested\":3295},\"both\":{\"degrader\":3835,\"negative_only\":492,\"inconclusive_only\":92,\"mixed_nonpositive\":6,\"untested\":136,\"tested\":4425}}"), 
    timing = c(redirect = 0, namelookup = 4e-05, connect = 4e-05, 
    pretransfer = 0.000152, starttransfer = 0.209845, total = 0.209952
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
