filter_column_for_n_most_values <- function(tbl, column, n = 5){
  col <- rlang::ensym(column)

  values <- tbl %>%
    dplyr::group_by(!!col) %>%
    dplyr::summarise("count" = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count), !!col) %>%
    dplyr::slice(1:n) %>%
    dplyr::pull(column)

  dplyr::filter(tbl, !!col %in% values)
}
