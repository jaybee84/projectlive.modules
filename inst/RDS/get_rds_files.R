syn <- create_synapse_login()

incoming_data <- get_synapse_tbl(syn, "syn23364404")
saveRDS(incoming_data, "inst/RDS/incoming_data.rds")

files <- get_synapse_tbl(syn, "syn16858331")
saveRDS(files, "inst/RDS/files.rds")
