structure(list(method = "GET", url = "pdb/pathways?limit=500&offset=0", 
    status_code = 200L, headers = structure(list(`cache-control` = "public, no-cache", 
        `content-type` = "application/json", etag = "W/\"none-2377f654f33b7389b66b88b4\"", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `content-length` = "143"), class = "httr2_headers"), 
    body = charToRaw("{\"total\":1,\"items\":[{\"id\":1,\"plastic_id\":16,\"plastic_name\":\"PET\",\"name\":\"PET degradation (Pseudideonella sakaiensis 201-F6)\",\"step_count\":10}]}"), 
    timing = c(redirect = 0, namelookup = 3.9e-05, connect = 3.9e-05, 
    pretransfer = 0.00024, starttransfer = 0.089326, total = 0.089397
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
