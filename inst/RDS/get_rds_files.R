syn <- create_synapse_login()

# nf ----

incoming_data <- get_synapse_tbl(syn, "syn23364404")
saveRDS(incoming_data, "inst/RDS/nf_incoming_data.rds")

nf_studies <-get_synapse_tbl(syn, "syn16787123")
saveRDS(nf_studies, "inst/RDS/nf_studies.rds")

nf_files <-
  get_synapse_tbl(
    syn,
    "syn16858331",
    limit = 5000,
    columns = c(
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
      "tumorType",
      "species",
      "projectId",
      "createdOn"
    )
  ) %>%
  format_date_columns() %>%
  dplyr::select(-c("createdOn", "ROW_ID", "ROW_VERSION", "ROW_ETAG")) %>%
  dplyr::left_join(
    dplyr::select(
      nf_studies,
      "studyName",
      "studyLeads"
    ),
    by = "studyName"
  )

saveRDS(nf_files, "inst/RDS/nf_files.rds")

nf_publications <- get_synapse_tbl(syn, "syn16857542")
saveRDS(nf_publications, "inst/RDS/nf_publications.rds")

nf_tools <- get_synapse_tbl(syn, "syn16859448")
saveRDS(nf_tools, "inst/RDS/nf_tools.rds")

# csbc ----

csbc_studies <-
  projectlive.modules::get_synapse_tbl(
    syn,
    "syn21918972"
  ) %>%
  dplyr::select(-c("ROW_ID", "ROW_VERSION"))

saveRDS(csbc_studies, "inst/RDS/csbc_studies.rds")

csbc_files <-
  projectlive.modules::get_synapse_tbl(
    syn,
    "syn9630847",
    columns = c(
      "id",
      "assay",
      "diagnosis",
      "tumorType",
      "species",
      "grantName",
      "createdOn",
      "consortium",
      "organ",
      "experimentalStrategy"
    )
  ) %>%
  format_date_columns() %>%
  dplyr::select(-c("createdOn", "ROW_ID", "ROW_VERSION", "ROW_ETAG")) %>%
  dplyr::mutate( "accessType" = "PUBLIC") %>%
  dplyr::inner_join(
    dplyr::select(csbc_studies, "theme", "grantName"),
    by = "grantName",
  ) %>%
  dplyr::slice(1:20000L)

saveRDS(csbc_files, "inst/RDS/csbc_files.rds")


csbc_publications <-
  get_synapse_tbl(
    syn,
    "syn21868591",
    columns = c(
      "grantName",
      "publicationId",
      "publicationYear"
    )
  ) %>%
  dplyr::mutate(
    "publicationYear" = as.integer(.data$publicationYear)
  )

saveRDS(csbc_publications, "inst/RDS/csbc_publications.rds")



csbc_tools <- get_synapse_tbl(syn, "syn21930566")
saveRDS(csbc_tools, "inst/RDS/csbc_tools.rds")
