
#' Download Source Data
#'
#' Download the source datasets to build the VIVID Site Map
#'
#' @param data_raw_directory Character vector of length one, the path of the data-raw directory
#' @param data_directory Character vector of length one, the path of the data directory
#'
#' @return Logical vector of length one with value TRUE on success.
#' @export
downloadSourceData <- function(data_raw_directory = "data-raw",
                               data_directory = "data") {

  if(!dir.exists(data_raw_directory)) dir.create(data_raw_directory)
  if(!dir.exists(data_directory)) dir.create(data_directory)

  # postcode lookup (NSPL 2026 Feb)

  postcode_to_bng_msoa21_lookup_filepath <- downloadExtractZipFile(url = "https://www.arcgis.com/sharing/rest/content/items/36b718ad00de49afb9ad364f8b815b9e/data",
                                                            unzip_directory = data_raw_directory,
                                                            unzip_files = "Data/NSPL_FEB_2026_UK.csv")

  postcode_to_bng_msoa21_lookup <- data.table::fread(postcode_to_bng_msoa21_lookup_filepath,
                                              header = TRUE,
                                              select = c(3,7:8,23),
                                              col.names = c("postcode",
                                                            "oseast1m",
                                                            "osnrth1m",
                                                            "msoa21"))

  saveRDS(postcode_to_bng_msoa21_lookup,
          file = paste0(data_directory,
                        "/postcode_to_bng_msoa21_lookup.rds"))

  rm(postcode_to_bng_msoa21_lookup,
     postcode_to_bng_msoa21_lookup_filepath)



  # MSOA 2021 boundaries

  england_wales_msoa21_goem_filepath <- downloadArcGISData("6b282db29762450881ed5159259a6e4e",
                                                               directory_path = data_raw_directory)

  england_wales_msoa21_goem <- sf::st_read(england_wales_msoa21_goem_filepath,
                                         query = "SELECT * FROM MSOA_2021_EW_BGC_V3")

  saveRDS(england_wales_msoa21_goem,
          file = paste0(data_directory,
                        "/england_wales_msoa21_goem.rds"))

  rm(england_wales_msoa21_goem,
     england_wales_msoa21_goem_filepath)

  # UK Countries (December 2024) boundaries

  uk_countries_goem_filepath <- downloadArcGISData("6f18dfc308d04372929dea6afa44b2c7",
                                                         directory_path = data_raw_directory)

  uk_countries_goem <- sf::st_read(uk_countries_goem_filepath,
                                   query = "SELECT * FROM CTRY_DEC_2024_UK_BSC")

  saveRDS(uk_countries_goem,
          file = paste0(data_directory,
                        "/uk_countries_goem.rds"))

  rm(uk_countries_goem,
     uk_countries_goem_filepath)

  # Towns and Cities (December 2015) boundaries

  towns_cities_goem_filepath <- downloadArcGISData("63a109c64a64410488d39c886152c162",
                                                         directory_path = data_raw_directory)

  towns_cities_goem <- sf::st_read(towns_cities_goem_filepath,
                                   query = "SELECT * FROM TCITY_2015_EW_BGG")

  saveRDS(towns_cities_goem,
          file = paste0(data_directory,
                        "/towns_cities_goem.rds"))

  rm(towns_cities_goem,
     towns_cities_goem_filepath)

  # ONS mid-year 2022 MSOA (2021) population estimates

  utils::download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimatesnationalstatistics/mid2022revisednov2025tomid2024/sapemsoaquinaryage20222024.xlsx",
                       paste0(data_raw_directory,
                              "/sapemsoaquinaryage20222024.xlsx"),
                       headers = c("User-Agent" = getUserAgent()))

  msoa21_2024_population_filename <- paste0(data_raw_directory,
                                    "/sapemsoaquinaryage20222024.xlsx")

  msoa21_2024_population <- openxlsx::read.xlsx(msoa21_2024_population_filename,
                                                sheet = "Mid-2024 MSOA 2021",
                                                startRow = 4,
                                                cols = c(3,5)) |>
    data.table::setDT()

  data.table::setnames(msoa21_2024_population,
                       c("MSOA.2021.Code",
                         "Total"),
                       c("msoa21",
                         "population"))

  saveRDS(msoa21_2024_population,
          file = paste0(data_directory,
                        "/msoa21_2024_population.rds"))

  rm(msoa21_2024_population,
     msoa21_2024_population_filename)


  # OHID MSOA 2026 catchment populations

  utils::download.file("https://assets.publishing.service.gov.uk/media/69fdb80d2a6137e93226b8ac/nhs-acute-hospital-trust-catchment-populations-data_tables-april-2026.ods",
                       paste0(data_raw_directory,
                              "/nhs-acute-hospital-trust-catchment-populations-data_tables-april-2026.ods"),
                       headers = c("User-Agent" = getUserAgent()))


  nhs_trusts_catchment_areas_2026_filename <- paste0(data_raw_directory,
                                                     "/nhs-acute-hospital-trust-catchment-populations-data_tables-april-2026.ods")

  nhs_trusts_catchment_areas_2026 <- readODS::read_ods(nhs_trusts_catchment_areas_2026_filename,
                                                       sheet = "Emergency",
                                                       skip = 2,
                                                       as_tibble = FALSE) |>
    data.table::setDT()
  gc()

  data.table::setnames(nhs_trusts_catchment_areas_2026,
                       make.names(colnames(nhs_trusts_catchment_areas_2026),
                                  unique = TRUE))

  nhs_trusts_catchment_areas_2026_fields <- data.frame(
    old_names = c("Catchment..year",
                  "Trust..code",
                  "Trust..name",
                  "MSOA21CD",
                  "First.past..the.post..FPTP."),
    new_names = c("year",
                  "ods_code",
                  "trust_name",
                  "msoa21",
                  "largest_share_in_msoa")
  )

  nhs_trusts_catchment_areas_2026_fields_to_remove <-
    colnames(nhs_trusts_catchment_areas_2026)[
      !(colnames(nhs_trusts_catchment_areas_2026) %in% nhs_trusts_catchment_areas_2026_fields$old_names)
    ]

  nhs_trusts_catchment_areas_2026[, (nhs_trusts_catchment_areas_2026_fields_to_remove) := NULL]

  data.table::setnames(
    nhs_trusts_catchment_areas_2026,
    nhs_trusts_catchment_areas_2026_fields$old_names,
    nhs_trusts_catchment_areas_2026_fields$new_names
  )

  saveRDS(nhs_trusts_catchment_areas_2026,
          file = paste0(data_directory,
                        "/nhs_trusts_catchment_areas_2026.rds"))

  rm(nhs_trusts_catchment_areas_2026,
     nhs_trusts_catchment_areas_2026_fields,
     nhs_trusts_catchment_areas_2026_fields_to_remove)


  nhs_trusts_patients <- readODS::read_ods(nhs_trusts_catchment_areas_2026_filename,
                                           sheet = "All_admissions",
                                           skip = 2,
                                           as_tibble = FALSE) |>
    data.table::setDT()
  gc()

  data.table::setnames(nhs_trusts_patients,
                       make.names(colnames(nhs_trusts_patients),
                                  unique = TRUE))

  nhs_trusts_patients_fields <- data.frame(
    old_names = c("Catchment..year",
                  "Trust..code",
                  "Trust..name",
                  "MSOA21CD",
                  "Patients..admitted",
                  "First.past..the.post..FPTP."),
    new_names = c("year",
                  "ods_code",
                  "trust_name",
                  "msoa21",
                  "msoa_trust_patients_3years",
                  "largest_share_in_msoa")
  )

  nhs_trusts_patients_fields_to_remove <-
    colnames(nhs_trusts_patients)[
      !(colnames(nhs_trusts_patients) %in% nhs_trusts_patients_fields$old_names)
    ]

  nhs_trusts_patients[, (nhs_trusts_patients_fields_to_remove) := NULL]

  data.table::setnames(
    nhs_trusts_patients,
    nhs_trusts_patients_fields$old_names,
    nhs_trusts_patients_fields$new_names
  )

  saveRDS(nhs_trusts_patients,
          file = paste0(data_directory,
                        "/nhs_trusts_patients.rds"))

  rm(nhs_trusts_patients,
     nhs_trusts_patients_fields,
     nhs_trusts_patients_fields_to_remove,
     nhs_trusts_catchment_areas_2026_filename)


  # OHID MSOA 2022 catchment populations - for Frimley
  # Download from https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl/file/976234504165

  nhs_trusts_catchment_areas_2022_filename <- paste0(data_raw_directory,
                                                "/2022 Trust Catchment Populations_Supplementary MSOA Analysis.xlsx")

  nhs_trusts_catchment_areas_2022 <- openxlsx::read.xlsx(nhs_trusts_catchment_areas_2022_filename,
                                                    sheet = "Emergency",
                                                    cols = c(1,3:5,11)) |>
    data.table::setDT()

  data.table::setnames(nhs_trusts_catchment_areas_2022,
                       c("CatchmentYear",
                         "msoa",
                         "TrustCode",
                         "TrustName",
                         "FPTP"),
                       c("year",
                         "msoa11",
                         "ods_code",
                         "trust_name",
                         "largest_share_in_msoa"))

  saveRDS(nhs_trusts_catchment_areas_2022,
          file = paste0(data_directory,
                        "/nhs_trusts_catchment_areas_2022.rds"))

  rm(nhs_trusts_catchment_areas_2022,
     nhs_trusts_catchment_areas_2022_filename)


  # MSOA2011 to MSOA2021 lookup (exact fit)

  msoa11_to_msoa21_lookup_filepath <- downloadArcGISData("fe04322006bd47bbb5f9a784b05d87da",
                                                        directory_path = data_raw_directory,
                                                        export_type = "csv")

  msoa11_to_msoa21_lookup <- data.table::fread(msoa11_to_msoa21_lookup_filepath,
                                              header = TRUE,
                                              select = c(1, 4),
                                              col.names = c("msoa11",
                                                            "msoa21"))

  saveRDS(msoa11_to_msoa21_lookup,
          file = paste0(data_directory,
                        "/msoa11_to_msoa21_lookup.rds"))

  rm(msoa11_to_msoa21_lookup,
     msoa11_to_msoa21_lookup_filepath)



  # MSOA2021 to LAD2022 lookup
  msoa21_to_lad22_lookup_filepath <- downloadArcGISData("b9ca90c10aaa4b8d9791e9859a38ca67",
                                                        directory_path = data_raw_directory,
                                                        export_type = "csv")

  msoa21_to_lad22_lookup <- data.table::fread(msoa21_to_lad22_lookup_filepath,
                                              header = TRUE,
                                              select = c(2, 5, 8),
                                              col.names = c("lsoa21",
                                                            "msoa21",
                                                            "lad22"))

  saveRDS(msoa21_to_lad22_lookup,
          file = paste0(data_directory,
                        "/msoa21_to_lad22_lookup.rds"))

  rm(msoa21_to_lad22_lookup,
     msoa21_to_lad22_lookup_filepath)

  # ONS internal migration data 2024

  utils::download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/internalmigrationinenglandandwales/detailedinternalmigrationestimates20242023localauthorities/detailedestimates2024on2023las.xlsx",
                       paste0(data_raw_directory,
                              "/detailedestimates2024on2023las.xlsx"),
                       headers = c("User-Agent" = getUserAgent()))

  lad23_2024_internal_migration_filename <- paste0(data_raw_directory,
                                                   "/detailedestimates2024on2023las.xlsx")

  lad23_2024_internal_migration <- openxlsx::read.xlsx(lad23_2024_internal_migration_filename,
                                                sheet = "IM2024 on 2023 LAs") |>
    data.table::setDT()

  age_cols <- colnames(lad23_2024_internal_migration)[substr(colnames(lad23_2024_internal_migration), 1, 4) == "Age_"]

  lad23_2024_internal_migration[, all_ages := Reduce(`+`, .SD),
                                .SDcols = age_cols]
  lad23_2024_internal_migration[, c(age_cols, "year") := NULL]
  lad23_2024_internal_migration <- lad23_2024_internal_migration[, .(people = sum(all_ages)),
                                                                 by = .(outla,
                                                                        inla)]

  data.table::setnames(lad23_2024_internal_migration,
                       c("outla",
                         "inla"),
                       c("lad23_out",
                         "lad23_in"))

  saveRDS(lad23_2024_internal_migration,
          file = paste0(data_directory,
                        "/lad23_2024_internal_migration.rds"))

  rm(lad23_2024_internal_migration,
     lad23_2024_internal_migration_filename)

  # NHS Trust code, name and postcode data

  nhs_acute_trusts <- jsonlite::fromJSON("https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?_format=text/json&PrimaryRoleId=197&Limit=1000")[[1]] |>
    data.table::setDT()

  fields_to_retain <- data.table::data.table(old_name = c("Name",
                                                          "OrgId",
                                                          "PostCode",
                                                          "OrgLink"),
                                             new_name = c("trust_name",
                                                          "ods_code",
                                                          "postcode",
                                                          "ods_api_link"))

  fields_to_drop <- colnames(nhs_acute_trusts)[!(colnames(nhs_acute_trusts) %in% fields_to_retain$old_name)]

  nhs_acute_trusts[, (fields_to_drop) := NULL]
  data.table::setnames(nhs_acute_trusts,
                       fields_to_retain$old_name,
                       fields_to_retain$new_name)


  saveRDS(nhs_acute_trusts,
          file = paste0(data_directory,
                        "/nhs_acute_trusts.rds"))

  return(TRUE)
}
