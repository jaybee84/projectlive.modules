func <- function(){

  files <- "nf_files2" %>%
    get_rds_path() %>%
    readRDS() %>%
    dplyr::as_tibble() %>%
    dplyr::select("projectId", "assay", "reportMilestone") %>%
    dplyr::filter(projectId %in% c("syn11374337", "syn11374354")) %>%
    dplyr::group_by_at(dplyr::vars("projectId", "assay", "reportMilestone")) %>%
    dplyr::tally(name = "n_files") %>%
    dplyr::ungroup() %>%
    tidyr::replace_na(list(assay = "Not Annotated")) %>%
    tidyr::drop_na() %>%
    dplyr::select(
      "projectId",
      "reportMilestone",
      "assay",
      "n_files"
    )

  timeline <- "nf_incoming_data" %>%
    get_rds_path() %>%
    readRDS() %>%
    dplyr::as_tibble() %>%
    dplyr::filter(projectSynID == "syn11374337") %>%
    dplyr::select("projectSynID", "assay", "reportMilestone", "estimatedMinNumSamples") %>%
    dplyr::group_by_at(dplyr::vars("projectSynID", "reportMilestone")) %>%
    dplyr::group_split() %>%
    purrr::map(
      .,
      ~ dplyr::mutate(
        .x,
        "assay" = forcats::as_factor(.data$assay),
        "assay" = forcats::fct_expand(.data$assay, "Not Annotated"),
      )
    ) %>%
    purrr::map(
      .,
      ~tidyr::complete(
        .x,
        .data$assay,
        tidyr::nesting(projectSynID, reportMilestone)
      )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      "projectId" = "projectSynID",
      "reportMilestone",
      "assay",
      "min_files" = "estimatedMinNumSamples"
    )

  tbl <-
    dplyr::full_join(
      timeline,
      files,
      by = c("projectId", "reportMilestone", "assay")
    ) %>%
    tidyr::replace_na(list(n_files = 0, min_files = 0)) %>%
    dplyr::arrange(projectId, reportMilestone, assay)

}


