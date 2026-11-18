structure(list(method = "GET", url = "pdb/stats?evidence_scope=organism_or_enzyme", 
    status_code = 200L, headers = structure(list(`cache-control` = "public, no-cache", 
        `content-type` = "application/json", etag = "W/\"none-c56042e5e774d39ac350554f\"", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `content-length` = "216"), class = "httr2_headers"), 
    body = charToRaw("{\"plastics\":185,\"papers\":1418,\"pathways\":1,\"compounds\":196,\"microorganisms\":1436,\"microorganisms_whole_organism\":1130,\"microorganisms_via_enzyme\":306,\"proteins\":3835,\"proteins_wild_type\":853,\"proteins_variants\":2982}"), 
    timing = c(redirect = 0, namelookup = 4e-05, connect = 4.1e-05, 
    pretransfer = 0.000153, starttransfer = 0.188297, total = 0.188387
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
