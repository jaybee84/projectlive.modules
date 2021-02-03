# Study Summary Module UI

#' @title   study_summary_module_ui and study_summary_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname study_summary_module
#' @export 
study_summary_module_ui <- function(id){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = T),
      shinydashboard::dashboardSidebar(disable = T),
      shinydashboard::dashboardBody(
        shiny::fluidPage(
          shinydashboard::box(
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::textOutput(ns('funding_agency')),
          ),
          shinydashboard::box(
            title = "Participating Studies",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            DT::dataTableOutput(ns('study_table')),
          ),
          shinydashboard::box(
            title = "",
            status = "primary",
            solidHeader = F,
            width = 12,
            collapsible = FALSE,
            shinydashboard::infoBoxOutput(ns('study'), width = 12)
          ),
          shinydashboard::box(
            title = "Study Summary",
            status = "primary",
            solidHeader = T,
            width = 12,
            collapsible = FALSE,
            shiny::htmlOutput(ns('study_details'))
          ),
          shinydashboard::box(
            title = "Study Timeline",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('study_timeline_plot'))
          ),
          shinydashboard::box(
            title = "Data Focus",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('data_focus_plot'))
          ),
          shinydashboard::box(
            title = "Annotation Activity", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('annotation_activity'))
          ),
          shinydashboard::box(
            title = "Publication Status", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('publication_status'))
          )
        )
      )
    )
  )
}

# Study Summary Module Server

#' @rdname study_summary_module
#' @export
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
study_summary_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      merged_table <- shiny::reactive({
        
        shiny::req(data(), config())
        
        config <- purrr::pluck(
          config(),
          "modules",
          "study_summary",
          "outputs",
          "merged_table"
        )
        
        create_merged_table_with_config(data(), config())
        
      })
      
      study_table <- shiny::reactive({
        
        shiny::req(data(), config())
        
        config <- purrr::pluck(
          config(),
          "modules",
          "study_summary",
          "outputs",
          "study_table"
        ) 
        
        merged_table() %>% 
          dplyr::select_at(
            unlist(c(config$group_columns, config$count_columns))
          ) %>% 
          dplyr::group_by_at(unlist(config$group_columns))%>% 
          dplyr::summarise_at(
            unlist(config$count_columns), 
            dplyr::n_distinct,
            na.rm = T
          ) %>% 
          dplyr::ungroup() %>% 
          format_plot_data_with_config(config) %>% 
          dplyr::arrange(!!rlang::sym(config$id_column))
      })
      
      ##start making outputs
      output$funding_agency <- shiny::renderText({
        print(glue::glue(
          "You are now viewing studies moderated by {data()$selected_group}. 
      Please click on a row in the table below to select a study and view the details."
        ))
      })
      
      output$study_table <- DT::renderDataTable(
        base::as.data.frame(study_table()), 
        server = TRUE, 
        selection = 'single'
      )
      
      selected_study_name <- shiny::reactive({
        shiny::req(!is.null(input$study_table_rows_selected), config)
        
        column_name <- purrr::pluck(
          config,
          "modules",
          "study_summary",
          "outputs",
          "study_table",
          "id_column"
        ) 
        
        study_table() %>% 
          dplyr::slice(input$study_table_rows_selected) %>% 
          dplyr::pull(column_name)
      })
      
      output$study <- shinydashboard::renderInfoBox({
        shiny::req(selected_study_name())
        shinydashboard::infoBox(
          "You have selected",
          selected_study_name(),
          icon = shiny::icon("file"),
          color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
          fill = FALSE
        )
      })
      
      filtered_merged_table <- shiny::reactive({
        column <- purrr::pluck(
          config,
          "modules",
          "study_summary",
          "outputs",
          "merged_table",
          "filter_column"
        ) 
        shiny::req(merged_table(), selected_study_name())
        filter_list_column(merged_table(), column, selected_study_name()) 
      })
      
      output$annotation_activity <- plotly::renderPlotly({
        
        shiny::req(
          filtered_merged_table(), 
          config
        )
        
        config <- purrr::pluck(
          config,
          "modules",
          "study_summary",
          "outputs",
          "annotation_activity"
        )
        
        data <- filtered_merged_table() %>% 
          format_plot_data_with_config(config) %>% 
          create_plot_count_df(
            factor_columns   = config$plot$x, 
            complete_columns = c(config$plot$x, config$plot$facet)
          )
        
        validate(need(sum(data$Count) > 0, config$empty_table_message))
        
        create_plot_with_config(
          data, config, "create_annotation_activity_plot"
        ) 
      })
      
      output$data_focus_selection_ui <- shiny::renderUI({
        shiny::req(config)
        choices <- config %>% 
          purrr::pluck(
            "modules", 
            "study_summary", 
            "outputs", 
            "data_focus", 
            "plot",
            "fill"
          )
        shiny::selectizeInput(
          ns('data_focus_columns'),
          label = "Choose to view",
          choices = choices,
          selected = choices,
          multiple = T
        )
        
      })
      
      output$data_focus_plot <- plotly::renderPlotly({
        
        shiny::req(
          filtered_merged_table(), 
          config
        )
        
        config <- config %>% 
          purrr::pluck(
            "modules", 
            "study_summary", 
            "outputs", 
            "data_focus"
          ) 
        
        data_list <- filtered_merged_table() %>% 
          format_plot_data_with_config(config) %>% 
          create_data_focus_tables(config$plot$x, config$plot$fill)
        
        validate(need(length(data_list) > 0 , config$empty_table_message))
        
        create_data_focus_plots(data_list, config)
      })
      
      output$study_timeline_plot <- plotly::renderPlotly({
        shiny::req(filtered_merged_table(), config)
        
        config <- config %>% 
          purrr::pluck(
            "modules", 
            "study_summary", 
            "outputs", 
            "study_timeline"
          ) 
        
        data <- filtered_merged_table() %>%
          format_plot_data_with_config(config) %>% 
          tidyr::drop_na()
        
        validate(need(nrow(data) > 0 , config$empty_table_message))
        
        create_plot_with_config(
          data, config, "create_study_timeline_plot"
        )
      })
      
      output$publication_status <- plotly::renderPlotly({
        
        shiny::req(config, data(), selected_study_name())
        
        config <- purrr::pluck(
          config,
          "modules",
          "study_summary",
          "outputs",
          "publication_status"
        )
        
        data <- data() %>% 
          purrr::pluck("tables", config$table) %>% 
          filter_list_column(config$filter_column, selected_study_name()) %>% 
          format_plot_data_with_config(config)
        
        validate(need(nrow(data) > 0 , config$empty_table_message))
        
        create_plot_with_config(
          data,
          config,
          "create_publication_status_plot"
        )%>%
          plotly::layout(yaxis = list(range = c(0, 5)), autosize = T)
      })
      
      output$study_details <- shiny::renderText({
        
        shiny::req(filtered_merged_table(), config)
        
        config <- config %>% 
          purrr::pluck(
            "modules", 
            "study_summary", 
            "outputs", 
            "study_details"
          ) 
        
        filtered_merged_table() %>% 
          format_plot_data_with_config(config) %>% 
          dplyr::distinct() %>% 
          dplyr::mutate("Unique Study ID" = stringr::str_c(
            '<a href=',
            '"https://www.synapse.org/#!Synapse:',
            .data$`Unique Study ID`,
            '">', 
            .data$`Unique Study ID`,
            '</a>'
          )) %>% 
          tidyr::pivot_longer(dplyr::everything()) %>% 
          dplyr::mutate(
            "name" = stringr::str_to_title(.data$name),
            "name" = stringr::str_c("<b>", .data$name, "</b>")
          ) %>% 
          knitr::kable(
            "html", escape = FALSE, col.names = NULL, align = c('r', 'l')
          ) %>% 
          kableExtra::kable_styling("striped", full_width = T)
      })
      
    }
  )
}
