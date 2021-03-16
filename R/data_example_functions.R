get_nf_data <- function(){
  list(
    "tables" = list(
      "files" = "nf_files" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "publications" = "nf_publications" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "studies" = "nf_studies" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "tools" = "nf_tools" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "incoming_data" = "nf_incoming_data" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble()
    ),
    "selected_group" = "Example Group"
  )
}

get_nf_ntap_data <- function(){
  nf_data <- get_nf_data()

  nf_ntap_tables <- nf_data %>%
    purrr::pluck("tables") %>%
    purrr::map(filter_list_column, "fundingAgency", "NTAP")

  nf_ntap_data <-  nf_data
  nf_ntap_data$tables <- nf_ntap_tables
  nf_ntap_data$selected_group <- "NTAP"
  return(nf_ntap_data)
}

get_nf_gff_data <- function(){
  nf_data <- get_nf_data()

  nf_gff_tables <- nf_data %>%
    purrr::pluck("tables") %>%
    purrr::map(filter_list_column, "fundingAgency", "GFF")

  nf_gff_data <-  nf_data
  nf_gff_data$tables <- nf_gff_tables
  nf_gff_data$selected_group <- "GFF"
  return(nf_gff_data)
}

get_nf_ctf_data <- function(){
  nf_data <- get_nf_data()

  nf_ctf_tables <- nf_data %>%
    purrr::pluck("tables") %>%
    purrr::map(filter_list_column, "fundingAgency", "CTF")

  nf_ctf_data <-  nf_data
  nf_ctf_data$tables <- nf_ctf_tables
  nf_ctf_data$selected_group <- "CTF"
  return(nf_ctf_data)
}

get_nf_summary_snapshot_config <- function(){
  "nf_summary_snapshot_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_nf_study_summary_config <- function(){
  "nf_study_summary_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_nf_publication_status_config <- function(){
  "nf_publication_status_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_csbc_data <- function(){
  list(
    "tables" = list(
      "files" = "csbc_files" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "publications" = "csbc_publications" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "studies" = "csbc_studies" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "tools" = "csbc_tools" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble()
    )
  )
}

get_csbc_summary_snapshot_config <- function(){
  "csbc_summary_snapshot_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_csbc_study_summary_config <- function(){
  "csbc_study_summary_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_csbc_publication_status_config <- function(){
  "csbc_publication_status_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}
