create_info_box <- function(config, data){
  shinydashboard::infoBox(
    title = config$title,
    value = add_distinct_values_from_columns(
      data[["tables"]][[config$table]],
      config$columns
    ),
    icon = shiny::icon(config$icon),
    color = "light-blue",
    fill = TRUE
  )
}
