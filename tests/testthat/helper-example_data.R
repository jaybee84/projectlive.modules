nf_data <- get_nf_data()
nf_summary_snapshot_config   <- get_nf_summary_snapshot_config()
nf_study_summary_config      <- get_nf_study_summary_config()
nf_publication_status_config <- get_nf_publication_status_config()

nf_gff_tables <- nf_data %>%
  purrr::pluck("tables") %>%
  purrr::map(filter_list_column, "fundingAgency", "GFF")

nf_gff_data <-  nf_data
nf_gff_data$tables <- nf_gff_tables

csbc_data <- get_csbc_data()
csbc_summary_snapshot_config   <- get_csbc_summary_snapshot_config()
csbc_study_summary_config      <- get_csbc_study_summary_config()
csbc_publication_status_config <- get_csbc_publication_status_config()
