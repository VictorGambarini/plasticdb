structure(list(method = "GET", url = "pdb/search/grouped?limit=1&q=cutinase", 
    status_code = 200L, headers = structure(list(`cache-control` = "public, no-cache", 
        `content-type` = "application/json", etag = "W/\"none-76b1dff2f902bb42ec947dee\"", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `x-ratelimit-limit` = "200", 
        `x-ratelimit-remaining` = "199", `content-length` = "767"), class = "httr2_headers"), 
    body = charToRaw("{\"total\":117,\"scope\":null,\"groups\":{\"plastic\":{\"total\":0,\"more_query\":\"plastic:cutinase\",\"items\":[]},\"protein\":{\"total\":25,\"more_query\":\"protein:cutinase\",\"items\":[{\"id\":1,\"route_key\":\"PB24UZ\",\"name\":\"Cutinase g9562.t1\",\"entity_type\":\"protein\",\"rank\":0.304,\"extra\":\"Gene: g9562 · from Clonostachys rosea\",\"match\":null,\"detail_url\":null}]},\"microorganism\":{\"total\":0,\"more_query\":\"microorganism:cutinase\",\"items\":[]},\"paper\":{\"total\":92,\"more_query\":\"paper:cutinase\",\"items\":[{\"id\":1260,\"route_key\":\"1260\",\"name\":\"Ueda, H., Tabata, J., Seshime, Y., Masaki, K., Sameshima-Yamashita, Y., & Kitamoto, H. (2021). Cutinase-like biodegradab\",\"entity_type\":\"paper\",\"rank\":0.8127,\"extra\":\"Year: 2021\",\"match\":null,\"detail_url\":null}]}},\"plastic_matches\":[],\"suggestion\":null}"), 
    timing = c(redirect = 0, namelookup = 3.7e-05, connect = 3.7e-05, 
    pretransfer = 0.000146, starttransfer = 0.594863, total = 0.59497
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
