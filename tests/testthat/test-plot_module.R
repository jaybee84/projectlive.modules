
test_that("plot_module_ui", {
  expect_type(plot_module_ui("id", "Title"), "list")
})

test_that("plot_module_server_nf1", {
  shiny::testServer(
    plot_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = nf_summary_snapshot_config %>%
        purrr::pluck("initiative_activity") %>%
        shiny::reactiveVal(),
      "plot_func" = shiny::reactiveVal("create_initiative_activity_plot")
    ),
    {
      expect_type(plot_data(), "list")
      expect_type(output$plot, "character")
    }
  )
})

test_that("plot_module_server_nf2", {
  shiny::testServer(
    plot_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = nf_publication_status_config %>%
        purrr::pluck("publication_status") %>%
        shiny::reactiveVal(),
      "plot_func" = shiny::reactiveVal("create_publication_status_plot")
    ),
    {
      expect_type(plot_data(), "list")
      expect_type(output$plot, "character")
    }
  )
})

test_that("plot_module_server_csbc1", {
  shiny::testServer(
    plot_module_server,
    args = list(
      "data" = shiny::reactiveVal(csbc_data),
      "config" = csbc_summary_snapshot_config %>%
        purrr::pluck("initiative_activity") %>%
        shiny::reactiveVal(),
      "plot_func" = shiny::reactiveVal("create_initiative_activity_plot")
    ),
    {
      expect_type(plot_data(), "list")
      expect_type(output$plot, "character")
    }
  )
})

test_that("plot_module_server_csbc2", {
  shiny::testServer(
    plot_module_server,
    args = list(
      "data" = shiny::reactiveVal(csbc_data),
      "config" = csbc_publication_status_config %>%
        purrr::pluck("publication_disease") %>%
        shiny::reactiveVal(),
      "plot_func" = shiny::reactiveVal("create_publication_disease_plot")
    ),
    {
      expect_type(plot_data(), "list")
      expect_type(output$plot, "character")
    }
  )
})
