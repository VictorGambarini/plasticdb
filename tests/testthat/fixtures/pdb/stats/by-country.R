structure(list(method = "GET", url = "pdb/stats/by-country", 
    status_code = 200L, headers = structure(list(`cache-control` = "public, no-cache", 
        `content-encoding` = "gzip", `content-type` = "application/json", 
        etag = "W/\"none-8a5bac2e433d909e160e6474\"", link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        vary = "Accept-Encoding", `x-plasticdb-release-source` = "live", 
        `content-length` = "1745"), class = "httr2_headers"), 
    body = charToRaw("{\"microorganisms\":[{\"country\":\"China\",\"count\":791},{\"country\":\"South Korea\",\"count\":225},{\"country\":\"Japan\",\"count\":168}],\"proteins\":[{\"country\":\"China\",\"count\":143},{\"country\":\"Japan\",\"count\":85},{\"country\":\"Slovakia\",\"count\":66}],\"plastics\":[{\"country\":\"South Korea\",\"count\":27},{\"country\":\"China\",\"count\":25},{\"country\":\"Japan\",\"count\":21}],\"papers\":[{\"country\":\"China\",\"count\":319},{\"country\":\"Japan\",\"count\":187},{\"country\":\"Germany\",\"count\":141}]}"), 
    timing = c(redirect = 0, namelookup = 3.8e-05, connect = 3.8e-05, 
    pretransfer = 0.000146, starttransfer = 0.089833, total = 0.09004
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
