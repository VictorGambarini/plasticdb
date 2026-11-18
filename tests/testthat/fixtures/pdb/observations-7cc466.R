structure(list(method = "GET", url = "pdb/observations?limit=2&offset=0", 
    status_code = 200L, headers = structure(list(`cache-control` = "public, no-cache", 
        `content-type` = "application/json", etag = "W/\"none-416995ad64c5899f0307413c\"", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `x-ratelimit-limit` = "200", 
        `x-ratelimit-remaining` = "197", `content-length` = "892"), class = "httr2_headers"), 
    body = charToRaw("{\"total\":101297,\"items\":[{\"id\":1,\"experiment_id\":1,\"interaction_id\":1,\"paper_id\":1,\"measurement_type\":\"clear_zone_diameter\",\"measurement_category\":\"plate_assay\",\"value\":1048,\"unit\":\"mm^2\",\"text_description\":\"Mean clear-zone (degradation) area of 1,048 mm2 (+/-53 SD) on M9 + PCL emulsion agar over 18 days; reported as halo area, not diameter.\",\"time_point\":null,\"time_point_unit\":null,\"sd\":null,\"n\":null,\"control_value\":null,\"products_summed\":false,\"product_compound_ids\":[]},{\"id\":2,\"experiment_id\":2,\"interaction_id\":1,\"paper_id\":1,\"measurement_type\":\"weight_loss\",\"measurement_category\":\"gravimetric\",\"value\":59.25,\"unit\":\"%\",\"text_description\":\"59.25% (SD 7.10%) weight loss of PCL film after 180 days at 28 C in M9 medium alone.\",\"time_point\":null,\"time_point_unit\":null,\"sd\":null,\"n\":null,\"control_value\":null,\"products_summed\":false,\"product_compound_ids\":[]}],\"offset\":0,\"limit\":2}"), 
    timing = c(redirect = 0, namelookup = 4.2e-05, connect = 4.2e-05, 
    pretransfer = 0.000158, starttransfer = 0.597609, total = 0.597739
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
