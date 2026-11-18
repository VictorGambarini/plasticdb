structure(list(method = "GET", url = "pdb/proteins?limit=1&offset=0&sort_by=nope", 
    status_code = 422L, headers = structure(list(`content-type` = "application/json", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `content-length` = "255"), class = "httr2_headers"), 
    body = charToRaw("{\"detail\":[{\"type\":\"literal_error\",\"loc\":[\"query\",\"sort_by\"],\"msg\":\"Input should be 'year', 'name', 'accession', 'id', 'plasticdb_id' or 'genbank_id'\",\"input\":\"nope\",\"ctx\":{\"expected\":\"'year', 'name', 'accession', 'id', 'plasticdb_id' or 'genbank_id'\"}}]}"), 
    timing = c(redirect = 0, namelookup = 3.7e-05, connect = 3.7e-05, 
    pretransfer = 0.000152, starttransfer = 0.042708, total = 0.042826
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
