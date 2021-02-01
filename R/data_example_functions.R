example_tables <- function(){
  list(
    "tables" = list(
      "files" = "files" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "publications" = "publications" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "studies" = "studies" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "tools" = "tools" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble()
    ),
    "selected_group" = "Example Group"
  )
}

example_summary_snapshot_module_config <- function(){
  "summary_snapshot_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}
