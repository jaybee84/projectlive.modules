nf_example_data <- function(){
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
        dplyr::as_tibble()
    ),
    "selected_group" = "Example Group"
  )
}

nf_example_summary_snapshot_config <- function(){
  "nf_summary_snapshot_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

csbc_example_data <- function(){
  list(
    "tables" = list(
      "files" = "csbc_files" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble()
    )
  )
}

csbc_example_summary_snapshot_config <- function(){
  "csbc_summary_snapshot_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}
