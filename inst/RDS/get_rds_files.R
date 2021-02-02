syn <- create_synapse_login()

# nf ----

incoming_data <- get_synapse_tbl(syn, "syn23364404")
saveRDS(incoming_data, "inst/RDS/nf_incoming_data.rds")

nf_files <-
  get_synapse_tbl(syn, "syn16858331") %>%
  format_date_columns() %>%
  dplyr::select(
    "id",
    "individualID",
    "specimenID",
    "assay",
    "consortium",
    "dataType",
    "fileFormat",
    "resourceType",
    "studyName",
    "accessType",
    "initiative",
    "year",
    "month"
  )


saveRDS(nf_files, "inst/RDS/nf_files.rds")

nf_publications <- get_synapse_tbl(syn, "syn16857542")
saveRDS(nf_publications, "inst/RDS/nf_publications.rds")

nf_studies <- get_synapse_tbl(syn, "syn16787123")
saveRDS(nf_studies, "inst/RDS/nf_studies.rds")

nf_tools <- get_synapse_tbl(syn, "syn16859448")
saveRDS(nf_tools, "inst/RDS/nf_tools.rds")

# csbc ----

csbc_files <-
  get_synapse_tbl(syn, "syn9630847") %>%
  dplyr::slice(1:5000) %>%
  format_date_columns() %>%
  dplyr::select(
    "id", "dataset", "assay", "Theme", "consortium", "grantName", "year", "month"
  ) %>%
  dplyr::mutate("accessType" = "PUBLIC")


saveRDS(csbc_files, "inst/RDS/csbc_files.rds")
