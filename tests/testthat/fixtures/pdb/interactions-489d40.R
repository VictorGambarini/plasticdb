structure(list(method = "GET", url = "pdb/interactions?limit=2&offset=0&plastic=PET&protein=PBFCTW", 
    status_code = 200L, headers = structure(list(`cache-control` = "public, no-cache", 
        `content-type` = "application/json", etag = "W/\"none-aac0b76a4bfbe693ce040ddf\"", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `x-ratelimit-limit` = "200", 
        `x-ratelimit-remaining` = "197", `content-length` = "987"), class = "httr2_headers"), 
    body = charToRaw("{\"total\":1152,\"items\":[{\"id\":9,\"paper_id\":2,\"plastic_id\":16,\"actor_type\":\"protein_variant\",\"result\":\"positive\",\"strain_occurrence_id\":null,\"microorganism_id\":null,\"microorganism_accession\":null,\"protein_id\":null,\"protein_accession\":null,\"protein_variant_id\":1,\"protein_variant_accession\":\"PBVPYT\",\"credited_microorganism_id\":null,\"experimental_system\":\"cell_free_enzyme\",\"enzyme_production_context\":\"heterologous\",\"enzyme_preparation\":\"purified\",\"strain_designation\":null,\"genetic_background\":null},{\"id\":10,\"paper_id\":2,\"plastic_id\":16,\"actor_type\":\"protein\",\"result\":\"positive\",\"strain_occurrence_id\":null,\"microorganism_id\":null,\"microorganism_accession\":null,\"protein_id\":3,\"protein_accession\":\"PBFCTW\",\"protein_variant_id\":null,\"protein_variant_accession\":null,\"credited_microorganism_id\":2,\"experimental_system\":\"cell_free_enzyme\",\"enzyme_production_context\":\"heterologous\",\"enzyme_preparation\":\"purified\",\"strain_designation\":null,\"genetic_background\":null}],\"offset\":0,\"limit\":2}"), 
    timing = c(redirect = 0, namelookup = 4.6e-05, connect = 4.6e-05, 
    pretransfer = 0.000159, starttransfer = 0.079426, total = 0.079502
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
