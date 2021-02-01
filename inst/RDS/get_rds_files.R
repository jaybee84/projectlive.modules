syn <- create_synapse_login()

incoming_data <- get_synapse_tbl(syn, "syn23364404")
saveRDS(incoming_data, "inst/RDS/incoming_data.rds")

files <-
  get_synapse_tbl(syn, "syn16858331") %>%
  dplyr::rename("initiative" = "consortium") %>%
  format_date_columns()
saveRDS(files, "inst/RDS/files.rds")

publications <- get_synapse_tbl(syn, "syn16857542")
saveRDS(publications, "inst/RDS/publications.rds")

studies <- get_synapse_tbl(syn, "syn16787123")
saveRDS(studies, "inst/RDS/studies.rds")

tools <- get_synapse_tbl(syn, "syn16859448")
saveRDS(tools, "inst/RDS/tools.rds")
