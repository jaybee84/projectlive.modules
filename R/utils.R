get_rds_path <- function(name, extension = ".rds"){
  name %>%
    stringr::str_c(extension) %>%
    file.path(system.file("RDS", package = "projectlive.modules"), .)
}
