syn <- create_synapse_login()

# nf ----


nf_studies <-get_synapse_tbl(syn, "syn16787123")
saveRDS(nf_studies, "inst/RDS/nf_studies.rds")

incoming_data <-
  get_synapse_tbl(
    syn,
    "syn23364404",
    columns = c(
      "fileFormat",
      "date_uploadestimate",
      "reportMilestone",
      "estimatedMinNumSamples",
      "fundingAgency",
      "projectSynID",
      "dataType"
    ),
    col_types = readr::cols(
      "estimatedMinNumSamples" = readr::col_integer(),
      "reportMilestone" = readr::col_integer()
    )
  ) %>%
  dplyr::left_join(
    dplyr::select(nf_studies, "studyName", "studyId"),
    by = c("projectSynID" = "studyId")
  ) %>%
  dplyr::mutate(
    "date_uploadestimate" = lubridate::mdy(date_uploadestimate),
  ) %>%
  dplyr::select(-"projectSynID") %>%
  dplyr::filter(
    !is.na(.data$date_uploadestimate) | !is.na(.data$reportMilestone)
  ) %>%
  tidyr::unnest("fileFormat") %>%
  dplyr::group_by(
    .data$dataType,
    .data$fileFormat,
    .data$date_uploadestimate,
    .data$reportMilestone,
    .data$fundingAgency,
    .data$studyName
  ) %>%
  dplyr::summarise("estimatedMinNumSamples" = sum(.data$estimatedMinNumSamples)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    "estimatedMinNumSamples" = dplyr::if_else(
      is.na(.data$estimatedMinNumSamples),
      0L,
      .data$estimatedMinNumSamples
    )
  )

saveRDS(incoming_data, "inst/RDS/nf_incoming_data.rds")

nf_studies2 <- nf_studies %>%
  dplyr::select(
    "studyName",
    "studyLeads",
    "fundingAgency",
    "studyId"
  )

all_nf_files <-
  get_synapse_tbl(
    syn,
    "syn16858331",
    columns = c(
      "id",
      "name",
      "parentId",
      "individualID",
      "specimenID",
      "assay",
      "consortium",
      "dataType",
      "fileFormat",
      "resourceType",
      "accessType",
      "initiative",
      "tumorType",
      "species",
      "projectId",
      "reportMilestone",
      "createdOn",
      "benefactorId",
      "studyName"
    ),
    col_types = readr::cols(
      "consortium" = readr::col_character(),
      "reportMilestone" = readr::col_integer()
    )
  ) %>%
  format_date_columns() %>%
  dplyr::select(-c("createdOn"))



nf_files <- dplyr::bind_rows(

  nf_files1 <- dplyr::inner_join(all_nf_files, nf_studies2, by = "studyName"),

  nf_files2 <- all_nf_files %>%
    dplyr::filter(!id %in% nf_files1$id) %>%
    dplyr::select(-studyName) %>%
    dplyr::inner_join(nf_studies2, by = c("projectId" = "studyId"), keep = T),

  nf_files3 <- all_nf_files %>%
    dplyr::filter(!id %in% c(nf_files1$id, nf_files2$id)) %>%
    dplyr::select(-studyName) %>%
    dplyr::inner_join(nf_studies2, by = c("benefactorId" = "studyId"), keep = T),

  nf_files4 <- all_nf_files %>%
    dplyr::filter(!id %in% c(nf_files1$id, nf_files2$id, nf_files3$id)) %>%
    dplyr::select(-studyName) %>%
    dplyr::mutate(
      "studyName" = "NA due to older study structure",
      "studyLeads" = list("NA due to older study structure")
    )

)

saveRDS(nf_files, "inst/RDS/nf_files.rds")

nf_publications <-
  get_synapse_tbl(syn, "syn16857542") %>%
  dplyr::mutate("year" = as.factor(.data$year))

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
  )

saveRDS(csbc_files, "inst/RDS/csbc_files.rds")


csbc_publications <-
  get_synapse_tbl(
    syn,
    "syn21868591",
    columns = c(
      "grantName",
      "publicationId",
      "publicationYear",
      "tissue",
      "theme"
    )
  ) %>%
  dplyr::mutate(
    "publicationYear" = as.factor(.data$publicationYear)
  )

saveRDS(csbc_publications, "inst/RDS/csbc_publications.rds")



csbc_tools <- get_synapse_tbl(syn, "syn21930566")
saveRDS(csbc_tools, "inst/RDS/csbc_tools.rds")
