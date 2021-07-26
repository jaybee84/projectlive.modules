utils::globalVariables(c(".", "where", "OAUTH_LIST"))

get_system_path <- function(name, extension, folder){
  name %>%
    stringr::str_c(extension) %>%
    file.path(system.file(folder, package = "projectlive.modules"), .)
}

get_rds_path <- purrr::partial(
  get_system_path,
  extension = ".rds",
  folder = "RDS"
)

get_json_path <- purrr::partial(
  get_system_path,
  extension = ".json",
  folder = "JSON"
)
