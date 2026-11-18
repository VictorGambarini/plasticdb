structure(list(method = "GET", url = "pdb/observations/1", status_code = 200L, 
    headers = structure(list(`cache-control` = "public, no-cache", 
        `content-type` = "application/json", etag = "W/\"none-fe6b5bf3ab7571ca508b91be\"", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `content-length` = "453"), class = "httr2_headers"), 
    body = charToRaw("{\"id\":1,\"experiment_id\":1,\"interaction_id\":1,\"paper_id\":1,\"measurement_type\":\"clear_zone_diameter\",\"measurement_category\":\"plate_assay\",\"value\":1048,\"unit\":\"mm^2\",\"text_description\":\"Mean clear-zone (degradation) area of 1,048 mm2 (+/-53 SD) on M9 + PCL emulsion agar over 18 days; reported as halo area, not diameter.\",\"time_point\":null,\"time_point_unit\":null,\"sd\":null,\"n\":null,\"control_value\":null,\"products_summed\":false,\"product_compound_ids\":[]}"), 
    timing = c(redirect = 0, namelookup = 3.6e-05, connect = 3.6e-05, 
    pretransfer = 0.000156, starttransfer = 0.039333, total = 0.039457
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
