################################################################################
# Outliers shiny app script                                                    #
################################################################################

################################################################################
#' outliers_app function
#'
#' Interactive app to flag outliers, out_of_range data and visually detected
#' data flaws.
#'
#' This function launches a shiny app to visually manage flagging outliers,
#' out_of_range values and visual data flaws. Data values selected after
#' visual exploration of the data presented in the app are coded in a script
#' saved in the \code{Data/si_code/Lvl_2/visual_check} folder. This script
#' performs the data flagging needed and serves as a reproducible tool in case
#' of reconstruction of the complete process undergo by the site data.
#'
#' @family apps
#'
#' @return An interactive app to manage outliers
#'
#' @export
#'
#' @import shiny

# START
# Function declaration
out_app <- function(parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Needed misc

    # 0.1 site codes list
    site_list <- list.dirs('Data', full.names = FALSE, recursive = FALSE)

    # 0.2 libraries
    # require(sapfluxnetQC1)
    # require(shiny)

    # STEP 1
    # Defining the app
    shinyApp(

      # 1.1 UI
      ui = fluidPage(
        # app title
        titlePanel('Outliers, ranges and flaw values flagging'),

        # sidebar layout
        sidebarLayout(

          # sidebar to select site, tree and env_var
          sidebarPanel(
            selectInput('site_sel', 'Select site', site_list, site_list[1]),
            uiOutput('tree_env'),
            width = 3
          ),

          # main panel, tabbed
          mainPanel(
            tabsetPanel(

              # inspector tab
              tabPanel(
                "Inspector",

                # row with dygraph for visualization
                fluidRow(
                  column(
                    width = 12,
                    dygraphs::dygraphOutput("time_series", height = "250px")
                  ),

                  # row with table and selectors
                  fluidRow(

                    # out table column
                    column(
                      width = 6,
                      shiny::br(),
                      DT::dataTableOutput("out_table")
                    ),

                    # selectors column
                    column(
                      width = 6,
                      shiny::br(),
                      verbatimTextOutput("sel_rows")
                    )
                  )
                )
              ),

              # script tab
              tabPanel(
                "Script obtained"
              )
            )
          )
        )
      ),

      # 1.2 SERVER
      server = function(input, output) {

        # load the site SfnData object
        sfndataInput <- reactive({
          name <- paste0(input$site_sel, '.RData')
          load(file.path('Data', input$site_sel, 'Lvl_2', 'lvl_2_out_warn', name))
          eval(as.name(input$site_sel))
        })

        # tree and env input
        output$tree_env <- renderUI({
          sfndata <- sfndataInput()
          names_trees <- names(get_sapf(sfndata)[, -1])
          names_env <- names(get_env(sfndata)[, -1])
          names_sel <- c(names_trees, names_env)
          radioButtons('tree_env', 'Choose tree or env var',
                             choices = names_sel, selected = names_sel[1])
        })

        # dygraph output
        output$time_series <- dygraphs::renderDygraph({
          # data
          sfndata <- sfndataInput()
          sapf_data <- get_sapf(sfndata)
          sapf_flags <- get_sapf_flags(sfndata)
          env_data <- get_env(sfndata)
          env_flags <- get_env_flags(sfndata)
          sapf_data_out <- sapf_data
          env_data_out <- env_data

          # subsets with/without outliers
          for (i in 1:ncol(sapf_data)) {
            sapf_data[stringr::str_detect(sapf_flags[,i], 'OUT_WARN'), i] <- NA
            sapf_data_out[!stringr::str_detect(sapf_flags[,i], 'OUT_WARN'), i] <- NA
          }

          for (i in 1:ncol(env_data)) {
            env_data[stringr::str_detect(env_flags[,i], 'OUT_WARN'), i] <- NA
            env_data_out[!stringr::str_detect(env_flags[,i], 'OUT_WARN'), i] <- NA
          }

          names(sapf_data_out) <- paste0(names(sapf_data_out), '_out')
          names(env_data_out) <- paste0(names(env_data_out), '_out')

          # data joined
          data_dg <- dplyr::bind_cols(sapf_data, sapf_data_out[,-1],
                                      env_data[,-1], env_data_out[,-1])

          # clean a little
          rm(sapf_data, env_data, sapf_data_out,
             env_data_out, sapf_flags, env_flags)

          # dygraph
          var_names <- c(input$tree_env,
                         paste0(input$tree_env, '_out'))
          data_dg %>%
            dplyr::select_(var_names[1], var_names[2]) %>%
            xts::xts(order.by = data_dg$TIMESTAMP,
                     tz = attr(data_dg$TIMESTAMP, 'tzone')) %>%
            dygraphs::dygraph('time_series') %>%
            dygraphs::dySeries(var_names[1],
                               label = 'no_out', color = 'green') %>%
            dygraphs::dySeries(var_names[2],
                               label = 'out', color = 'red') %>%
            dygraphs::dyRangeSelector() %>%
            dygraphs::dyOptions(useDataTimezone = TRUE)
        })

        # outliers table
        # output$out_table <- renderTable({
        #   sfndata <- sfndataInput()
        #   get_sapf_flags(sfndata) %>%
        #     dplyr::select_('TIMESTAMP', input$tree_env) %>%
        # dplyr::filter_(lazyeval::interp(quote(x == "OUT_WARN"),
        #                                 x = as.name(input$tree_env))) %>%
        #     dplyr::mutate(TIMESTAMP = as.character(TIMESTAMP))
        # }, rownames = TRUE, striped = TRUE, spacing = "s", align = 'lcc')
        output$out_table <- DT::renderDataTable({
          sfndata <- sfndataInput()
          get_sapf_flags(sfndata) %>%
            dplyr::full_join(get_env_flags(sfndata), by = 'TIMESTAMP') %>%
            dplyr::mutate(index = rownames(.)) %>%
            dplyr::select_('index', 'TIMESTAMP', input$tree_env) %>%
            dplyr::filter_(lazyeval::interp(quote(x == "OUT_WARN"),
                                            x = as.name(input$tree_env))) %>%
            dplyr::mutate(TIMESTAMP = as.character(TIMESTAMP)) %>%
            DT::datatable(extensions = 'Scroller',
                          options = list(dom = 't',
                                         deferRender = TRUE,
                                         scrollY = 350,
                                         scroller = TRUE))
        })

        # selected rows
        output$sel_rows <- renderPrint({
          selected <- input$out_table_rows_selected
          sfndata <- sfndataInput()
          indexes <- get_sapf_flags(sfndata) %>%
            dplyr::full_join(get_env_flags(sfndata), by = 'TIMESTAMP') %>%
            dplyr::mutate(index = rownames(.)) %>%
            dplyr::select_('index', 'TIMESTAMP', input$tree_env) %>%
            dplyr::filter_(lazyeval::interp(quote(x == "OUT_WARN"),
                                            x = as.name(input$tree_env))) %>%
            dplyr::select_('index') %>%
            unlist()

          rows_selected <- indexes[selected]

          if (length(rows_selected)) {
            cat("Rows selected for outlier remove:\n\n")
            cat(rows_selected, sep = ', ')
          }
        })

      }
    )
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'out_app', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'out_app', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'out_app', sep = '.'))})
}
