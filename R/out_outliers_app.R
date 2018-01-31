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

    # 0.1 site codes list (only those passed already to LVL2)
    site_list <- names(
      sapfluxnetQC1::df_whos_ready_to('lvl2', 'done', parent_logger = parent_logger)
    )

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
                      width = 5,
                      shiny::br(),
                      shiny::h3('Possible outliers'),
                      DT::dataTableOutput("out_table")
                    ),

                    # selectors column
                    column(
                      width = 5,
                      shiny::br(),
                      shiny::h3('Selected outliers to remove'),
                      shiny::tableOutput("sel_rows")
                    ),

                    # button column
                    column(
                      width = 2,
                      shiny::br(),
                      shiny::h3('Write tables'),
                      shiny::actionButton("write_out", "Write outliers",
                                          icon = icon('pencil')),
                      shiny::br(),
                      shiny::actionButton("write_range", "Write out of range",
                                          icon = icon('pencil'))
                    )
                  )
                )
              ),

              # script tab
              tabPanel(
                "Outliers & Ranges to remove",

                fluidRow(
                  column(
                    4,
                    shiny::h3("Outliers table"),
                    shiny::br(),
                    DT::dataTableOutput("saved_out_table"),
                    shiny::br(),
                    shiny::actionButton("reset_out", "Reset table",
                                        icon = icon('eraser'))
                  ),

                  column(
                    4,
                    shiny::h3("Ranges table"),
                    shiny::br(),
                    DT::dataTableOutput("saved_range_table"),
                    shiny::br(),
                    shiny::actionButton("reset_range", "Reset table",
                                        icon = icon('eraser'))
                  ),

                  column(
                    4,
                    shiny::h3("Set status"),
                    shiny::br(),
                    shiny::actionButton("set_status", "Statutize!",
                                        icon = icon('gavel'))
                  )
                )
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

        # generate the outliers table
        out_table_gen <- reactive({
          sfndata <- sfndataInput()
          variable <- input$tree_env
          outliers_tab <- get_sapf_flags(sfndata) %>%
            dplyr::full_join(get_env_flags(sfndata), by = 'TIMESTAMP') %>%
            dplyr::mutate(index = rownames(.)) %>%
            dplyr::select_('index', 'TIMESTAMP', variable) %>%
            dplyr::filter_(lazyeval::interp(quote(stringr::str_detect(x, 'OUT_WARN') | stringr::str_detect(x, 'RANGE_WARN')),
                                            x = as.name(variable)))
          outliers_tab
        })

        # generate the selected outliers table
        selected_rows_gen <- reactive({
          selected <- input$out_table_rows_selected
          variable <- input$tree_env
          indexes <- out_table_gen() %>%
            dplyr::select_('index') %>%
            unlist()

          if (length(selected)) {
            rows_selected <- indexes[selected]
            indexes_out_rem <- data.frame(index = rows_selected,
                                          variable = variable,
                                          stringsAsFactors = FALSE)
            indexes_out_rem
          } else {
            data.frame(index = numeric(0),
                       variable = character(0))
          }
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
          sapf_data_range <- sapf_data
          env_data_range <- env_data

          # subsets with/without outliers and out of range values
          for (i in 1:ncol(sapf_data)) {
            sapf_data[stringr::str_detect(sapf_flags[,i], 'OUT_WARN'), i] <- NA
            sapf_data[stringr::str_detect(sapf_flags[,i], 'RANGE_WARN'), i] <- NA
            sapf_data_out[!stringr::str_detect(sapf_flags[,i], 'OUT_WARN'), i] <- NA
            sapf_data_range[!stringr::str_detect(sapf_flags[,i], 'RANGE_WARN'), i] <- NA
          }

          for (i in 1:ncol(env_data)) {
            env_data[stringr::str_detect(env_flags[,i], 'OUT_WARN'), i] <- NA
            env_data[stringr::str_detect(env_flags[,i], 'RANGE_WARN'), i] <- NA
            env_data_out[!stringr::str_detect(env_flags[,i], 'OUT_WARN'), i] <- NA
            env_data_range[!stringr::str_detect(env_flags[,i], 'RANGE_WARN'), i] <- NA
          }

          names(sapf_data_out) <- paste0(names(sapf_data_out), '_out')
          names(env_data_out) <- paste0(names(env_data_out), '_out')
          names(sapf_data_range) <- paste0(names(sapf_data_range), '_range')
          names(env_data_range) <- paste0(names(env_data_range), '_range')

          # data joined
          data_dg <- dplyr::bind_cols(sapf_data, sapf_data_out[,-1], sapf_data_range[,-1],
                                      env_data[,-1], env_data_out[,-1], env_data_range[,-1])

          # clean a little
          rm(sapf_data, env_data, sapf_data_out, env_data_out,
             sapf_data_range, env_data_range, sapf_flags, env_flags)

          # dygraph
          var_names <- c(input$tree_env,
                         paste0(input$tree_env, '_out'),
                         paste0(input$tree_env, '_range'))
          data_dg %>%
            dplyr::select_(var_names[1], var_names[2], var_names[3]) %>%
            xts::xts(order.by = data_dg$TIMESTAMP,
                     tz = attr(data_dg$TIMESTAMP, 'tzone')) %>%
            dygraphs::dygraph('time_series') %>%
            dygraphs::dySeries(var_names[1],
                               label = 'no_out', color = 'green') %>%
            dygraphs::dySeries(var_names[2],
                               label = 'out', color = 'red') %>%
            dygraphs::dySeries(var_names[3],
                               label = 'range', color = 'orange') %>%
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
          out_table_gen() %>%
            dplyr::mutate(TIMESTAMP = as.character(TIMESTAMP)) %>%
            DT::datatable(extensions = 'Scroller',
                          options = list(dom = 't',
                                         deferRender = TRUE,
                                         scrollY = 300,
                                         scroller = TRUE))
        })

        # selected rows
        output$sel_rows <- renderTable({
          selected_rows_gen()
        })

        # write outliers table when button is pressed
        observeEvent(
          eventExpr = input$write_out,
          handlerExpr = {
            table_to_write <- selected_rows_gen()
            file_name_out <- file.path(
              'Data', input$site_sel,
              'Lvl_2', 'lvl_2_out_warn',
              paste0(input$site_sel, '_out_to_remove.txt')
            )

            # if file is new add column names
            if (file.exists(file_name_out)) {
              write.table(table_to_write,
                          file = file_name_out,
                          append = TRUE, row.names = FALSE, col.names = FALSE)
            } else {
              write.table(table_to_write,
                          file = file_name_out,
                          append = TRUE, row.names = FALSE, col.names = TRUE)
            }
          }
        )

        # write ranges table when button is pressed
        observeEvent(
          eventExpr = input$write_range,
          handlerExpr = {
            table_to_write <- selected_rows_gen()
            file_name_out <- file.path(
              'Data', input$site_sel,
              'Lvl_2', 'lvl_2_out_warn',
              paste0(input$site_sel, '_ranges_to_remove.txt')
            )

            # if file is new add column names
            if (file.exists(file_name_out)) {
              write.table(table_to_write,
                          file = file_name_out,
                          append = TRUE, row.names = FALSE, col.names = FALSE)
            } else {
              write.table(table_to_write,
                          file = file_name_out,
                          append = TRUE, row.names = FALSE, col.names = TRUE)
            }
          }
        )

        # Outliers to remove table
        saved_out_table <- reactive({

          file_name_out <- file.path('Data', input$site_sel,
                                     'Lvl_2', 'lvl_2_out_warn',
                                     paste0(input$site_sel, '_out_to_remove.txt'))

          if (file.exists(file_name_out)) {
            read.table(file_name_out, header = TRUE) %>%
              DT::datatable(extensions = 'Scroller',
                            options = list(dom = 't',
                                           deferRender = TRUE,
                                           scrollY = 300,
                                           scroller = TRUE))
          }
        })

        output$saved_out_table <- DT::renderDataTable({
          saved_out_table()
        })

        # reset
        observeEvent(
          eventExpr = input$reset_out,
          handlerExpr = {
            file_name_out <- file.path('Data', input$site_sel,
                                       'Lvl_2', 'lvl_2_out_warn',
                                       paste0(input$site_sel, '_out_to_remove.txt'))

            unlink(file_name_out)
          }
        )

        # Outliers to remove table
        output$saved_range_table <- DT::renderDataTable({
          file_name_out <- file.path('Data', input$site_sel,
                                     'Lvl_2', 'lvl_2_out_warn',
                                     paste0(input$site_sel, '_ranges_to_remove.txt'))
          if (file.exists(file_name_out)) {
            read.table(file_name_out, header = TRUE) %>%
              DT::datatable(extensions = 'Scroller',
                            options = list(dom = 't',
                                           deferRender = TRUE,
                                           scrollY = 300,
                                           scroller = TRUE))
          }
        })

        # reset
        observeEvent(
          eventExpr = input$reset_range,
          handlerExpr = {
            file_name_out <- file.path('Data', input$site_sel,
                                       'Lvl_2', 'lvl_2_out_warn',
                                       paste0(input$site_sel, '_ranges_to_remove.txt'))

            unlink(file_name_out)
          }
        )

        # Write status button
        observeEvent(
          eventExpr = input$set_status,
          handlerExpr = {
            df_set_status(input$site_sel, LVL2 = list(TO_REM = 'READY'))
          }
        )
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
