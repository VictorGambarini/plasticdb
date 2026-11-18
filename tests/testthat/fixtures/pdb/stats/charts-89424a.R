structure(list(method = "GET", url = "pdb/stats/charts?evidence_scope=organism_or_enzyme", 
    status_code = 200L, headers = structure(list(`cache-control` = "public, no-cache", 
        `content-encoding` = "gzip", `content-type` = "application/json", 
        etag = "W/\"none-4c3b790a597328c8b27fb6d6\"", link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        vary = "Accept-Encoding", `x-plasticdb-release-source` = "live", 
        `content-length` = "749"), class = "httr2_headers"), 
    body = charToRaw("{\"environments\":[{\"code\":\"plastic_surface\",\"label\":\"Plastic surface\",\"count\":287},{\"code\":\"soil\",\"label\":\"Soil\",\"count\":275},{\"code\":\"plant_material\",\"label\":\"Plant material (wood, litter, leaves)\",\"count\":150}],\"enzyme_origins\":[{\"code\":\"soil\",\"label\":\"Soil\",\"count\":114},{\"code\":\"compost\",\"label\":\"Compost\",\"count\":28},{\"code\":\"marine_sediment\",\"label\":\"Marine sediment\",\"count\":25}],\"publications_per_year\":[{\"year\":1975,\"count\":2},{\"year\":1977,\"count\":1},{\"year\":1983,\"count\":1}],\"top_plastics\":[{\"label\":\"PET\",\"count\":372},{\"label\":\"Impranil\",\"count\":341},{\"label\":\"PCL\",\"count\":333}],\"current_year\":2026}"), 
    timing = c(redirect = 0, namelookup = 3.9e-05, connect = 3.9e-05, 
    pretransfer = 0.000156, starttransfer = 0.101431, total = 0.101577
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
