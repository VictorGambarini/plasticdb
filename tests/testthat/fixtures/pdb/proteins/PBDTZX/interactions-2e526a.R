structure(list(method = "GET", url = "pdb/proteins/PBDTZX/interactions?kind=all&limit=3&offset=0", 
    status_code = 404L, headers = structure(list(`content-type` = "application/json", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `content-length` = "308"), class = "httr2_headers"), 
    body = charToRaw("{\"detail\":\"PBDTZX is an engineered variant: variants have no /interactions sub-resource. Its record, with its interactions, kinetics and properties, is at /api/v1/variants/PBDTZX.\",\"accession\":\"PBDTZX\",\"kind\":\"engineered\",\"reason\":\"engineered_variant\",\"location\":\"/api/v1/variants/PBDTZX\",\"merged_into\":null}"), 
    timing = c(redirect = 0, namelookup = 3.6e-05, connect = 3.6e-05, 
    pretransfer = 0.000133, starttransfer = 0.024105, total = 0.024138
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
