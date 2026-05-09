library(data.table)
source("r/getNhsCatchmentAreas.r")

nhs_acute_trusts <- readRDS("data/nhs_acute_trusts.rds")
postcode_to_bng_msoa11_lookup <- readRDS("data/postcode_to_bng_msoa11_lookup.rds")

nhs_acute_trusts <- merge(nhs_acute_trusts,
                          postcode_to_bng_msoa11_lookup[, .(postcode, msoa11)],
                          by = "postcode",
                          all.x = TRUE)

rm(postcode_to_bng_msoa11_lookup)
gc()

# Remove Trust dissolved in 1999 (for which we can't find postcode/MSOA11)
vivid_included_nhs_acute_trusts <- nhs_acute_trusts[
  trust_name != "ALLINGTON NHS TRUST"
]

stopifnot(vivid_included_nhs_acute_trusts[is.na(msoa11), .N] == 0)

# Remove Trusts not in England
vivid_included_nhs_acute_trusts <- vivid_included_nhs_acute_trusts[
  substring(msoa11, 1, 1) == "E"
]

nhs_trust_contributing_to_datamart <- fread("data-raw/sites.csv")
vivid_catchment_areas <- getNhsCatchmentAreas("data-raw/sites.csv")

# Find any Trusts in the catchment areas (including those Trust for which the catchment areas are created)
vivid_included_nhs_acute_trusts <- merge(vivid_included_nhs_acute_trusts,
                                         vivid_catchment_areas[, .(msoa11, lab_area = site_name)],
                                         by = "msoa11",
                                         all.x = TRUE)

vivid_included_nhs_acute_trusts <- merge(vivid_included_nhs_acute_trusts,
                                         nhs_trust_contributing_to_datamart[, .(ods_code, lab = site_name)],
                                         by = "ods_code",
                                         all.x = TRUE)

vivid_included_nhs_acute_trusts <- vivid_included_nhs_acute_trusts[
  !(is.na(lab_area) & is.na(lab))
]

# For these, find end date
trust_close_dates_list <- sapply(vivid_included_nhs_acute_trusts$ods_api_link,
                                 function(link) {
                                   org_dates <- jsonlite::fromJSON(link)[[1]]$Date
                                   Sys.sleep(0.01)
                                   if(!("End" %in% colnames(org_dates)))
                                     return(as.character(NA))
                                   if("Legal" %in% org_dates[, "Type"])
                                     return(org_dates[org_dates$Type == "Legal", "End"])
                                   return(org_dates[org_dates$Type == "Operational", "End"])
                                 })

trust_close_dates <- data.table(
  ods_api_link = names(trust_close_dates_list),
  closure_date = as.Date(unlist(trust_close_dates_list))
)

vivid_included_nhs_acute_trusts <- merge(vivid_included_nhs_acute_trusts,
                                         trust_close_dates,
                                         by = "ods_api_link",
                                         all.x = TRUE)

vivid_included_nhs_acute_trusts <- vivid_included_nhs_acute_trusts[
  is.na(closure_date) | closure_date >= as.Date("2021-04-01")
]

# Remove the following community and/or mental health care providers
vivid_included_nhs_acute_trusts <- vivid_included_nhs_acute_trusts[
  !(trust_name %in% c("BERKSHIRE HEALTHCARE NHS FOUNDATION TRUST",
                      "BIRMINGHAM AND SOLIHULL MENTAL HEALTH NHS FOUNDATION TRUST",
                      "CAMBRIDGESHIRE AND PETERBOROUGH NHS FOUNDATION TRUST",
                      "CORNWALL PARTNERSHIP NHS FOUNDATION TRUST",
                      "CUMBRIA, NORTHUMBERLAND, TYNE AND WEAR NHS FOUNDATION TRUST",
                      "GREATER MANCHESTER MENTAL HEALTH NHS FOUNDATION TRUST",
                      "HAMPSHIRE AND ISLE OF WIGHT HEALTHCARE NHS FOUNDATION TRUST",
                      "LEEDS AND YORK PARTNERSHIP NHS FOUNDATION TRUST",
                      "LEEDS COMMUNITY HEALTHCARE NHS TRUST",
                      "LEICESTERSHIRE PARTNERSHIP NHS TRUST",
                      "NOTTINGHAMSHIRE HEALTHCARE NHS FOUNDATION TRUST",
                      "SOLENT NHS TRUST",
                      "TAVISTOCK AND PORTMAN NHS FOUNDATION TRUST"))
]

# Remove ambulance services
vivid_included_nhs_acute_trusts <- vivid_included_nhs_acute_trusts[
  !grepl(" AMBULANCE SERVICE ", trust_name)
]

# Remove specialist centres
vivid_included_nhs_acute_trusts <- vivid_included_nhs_acute_trusts[!(
  trust_name %in% c("ROYAL PAPWORTH HOSPITAL NHS FOUNDATION TRUST",
                    "QUEEN VICTORIA HOSPITAL NHS FOUNDATION TRUST",
                    "THE ROYAL ORTHOPAEDIC HOSPITAL NHS FOUNDATION TRUST",
                    "THE CHRISTIE NHS FOUNDATION TRUST")
)]

# Check those Trusts that have closed has a successor that is included
closed_trusts_successors_list <- sapply(vivid_included_nhs_acute_trusts[!is.na(closure_date), ods_api_link],
                                 function(link) {
                                   successors <- jsonlite::fromJSON(link)[[1]]$Succs$Succ
                                   Sys.sleep(0.01)

                                   if(is.null(successors))
                                     return("FAILED")

                                   successors <- successors[successors$Type == "Successor", "Target"]

                                   if(nrow(successors) == 0)
                                     return(as.character(NA))

                                   return(successors$OrgId$extension)
                                 })

closed_trusts_successors <- data.table(
  ods_api_link = rep(names(closed_trusts_successors_list),
                     times = sapply(closed_trusts_successors_list, length)),
  successor_trust = unlist(closed_trusts_successors_list)
)

closed_trusts_successors[, successor_is_included_trust := successor_trust %in% vivid_included_nhs_acute_trusts[is.na(closure_date), ods_code]]
closed_trusts_successors_agg <- closed_trusts_successors[
  ,
  .(successors_included = all(successor_is_included_trust)),
    by = ods_api_link
]

vivid_included_nhs_acute_trusts <- merge(vivid_included_nhs_acute_trusts,
                                         closed_trusts_successors_agg,
                                         by = "ods_api_link",
                                         all.x = TRUE)

stopifnot(vivid_included_nhs_acute_trusts[
  !is.na(closure_date) &
    (is.na(successors_included) | !successors_included)
  , .N
] == 0)


# Output included labs

vivid_included_nhs_acute_trusts[is.na(lab), lab := lab_area]
stopifnot(vivid_included_nhs_acute_trusts[is.na(lab), .N] == 0)

setorder(vivid_included_nhs_acute_trusts, lab, trust_name)
fwrite(vivid_included_nhs_acute_trusts[, .(`Laboratory network` = lab,
                                           `NHS England Organisation Data Service (ODS) name` = trust_name,
                                           `ODS health care provider code` = ods_code)],
       file = "data-out/vivid_included_nhs_trusts.csv")

