structure(list(method = "GET", url = "pdb/experiments/1", status_code = 200L, 
    headers = structure(list(`cache-control` = "public, no-cache", 
        `content-type` = "application/json", etag = "W/\"none-b157749a7541e075f55c8329\"", 
        link = "<https://creativecommons.org/licenses/by/4.0/>; rel=\"license\"", 
        `x-plasticdb-release-source` = "live", `content-length` = "855"), class = "httr2_headers"), 
    body = charToRaw("{\"id\":1,\"interaction_id\":1,\"paper_id\":1,\"method\":\"clear_zone\",\"method_category\":\"plate_assay\",\"temperature\":21,\"temperature_unit\":\"C\",\"ph\":null,\"buffer\":null,\"duration\":18,\"duration_unit\":\"days\",\"substrate_form\":null,\"sole_carbon_source\":true,\"culture_medium\":\"M9 minimal agar with emulsified PCL\",\"aerobicity\":\"aerobic\",\"substrate_crystallinity_pct\":null,\"substrate_mw\":14000,\"substrate_mw_unit\":\"g/mol\",\"analytical_grade\":true,\"detection_limit\":null,\"detection_limit_unit\":null,\"quantitation_ceiling\":null,\"quantitation_ceiling_unit\":null,\"initial_mass\":null,\"initial_mass_unit\":null,\"assay_result\":null,\"assay_result_notes\":null,\"notes\":\"Incubated at room temperature (18-24 C). C. rosea did not grow on agar-only medium, indicating PCL served as the sole carbon source. Clear degradation halo visible after 2 days.\",\"additional_conditions\":null}"), 
    timing = c(redirect = 0, namelookup = 3.9e-05, connect = 3.9e-05, 
    pretransfer = 0.000166, starttransfer = 0.075344, total = 0.075458
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
