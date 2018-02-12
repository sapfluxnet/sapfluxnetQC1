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
#' visual exploration of the data presented in the app are coded in files
#' saved in the \code{Data/si_code/Lvl_2/lvl_2_out_warn} folder. These files
#' are used later to substitute outliers, remove out of range values and
#' remove manual values in the \code{\link{df_warn_to_remove}} function.
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

    # 0.1 site codes list (only those passed already to LVL2 and not yet removed)
    site_list <- names(
      sapfluxnetQC1::df_whos_ready_to('lvl2', 'done', parent_logger = parent_logger)
    )

    already_rem_done_list <- names(
      sapfluxnetQC1::df_whos_ready_to('rem', 'done', parent_logger = parent_logger)
    )

    site_list <- site_list[!(site_list %in% already_rem_done_list)]

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
            width = 2
          ),

          # main panel, tabbed
          mainPanel(
            width = 10,
            tabsetPanel(

              # inspector tab
              tabPanel(
                "Inspector",

                # row with dygraph for visualization
                fluidRow(
                  column(
                    width = 10,
                    dygraphs::dygraphOutput("time_series", height = "275px")
                  ),

                  # button for selecting the displayed time frame
                  column(
                    width = 2,
                    shiny::br(),
                    shiny::p('Zoom to the desired time frame to remove and ',
                             'click the right button'),
                    shiny::br(),
                    shiny::h4('Remove data from: '),
                    shiny::textOutput('time_1'),
                    shiny::h4('to: '),
                    shiny::textOutput('time_2'),
                    shiny::br(),
                    shiny::actionButton('timeframe', 'Remove this time frame',
                                        icon('crosshairs'))
                  )
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
                    shiny::h3('Selected data to substitute/remove'),
                    shiny::tableOutput("sel_rows")
                  ),

                  # button column
                  column(
                    width = 2,
                    shiny::br(),
                    shiny::h3('Write tables'),
                    shiny::br(),
                    shiny::actionButton("write_out", "Write outliers",
                                        icon = icon('pencil')),
                    shiny::br(),
                    shiny::actionButton("write_range", "Write out of range",
                                        icon = icon('pencil'))
                  )
                )
              ),

              # script tab
              tabPanel(
                "Outliers & Ranges to remove",

                fluidRow(
                  column(
                    3,
                    shiny::h3("Outliers table"),
                    shiny::br(),
                    DT::dataTableOutput("saved_out_table"),
                    shiny::br(),
                    shiny::actionButton("reset_out", "Reset table",
                                        icon = icon('eraser'))
                  ),

                  column(
                    3,
                    shiny::h3("Ranges table"),
                    shiny::br(),
                    DT::dataTableOutput("saved_range_table"),
                    shiny::br(),
                    shiny::actionButton("reset_range", "Reset table",
                                        icon = icon('eraser'))
                  ),

                  column(
                    3,
                    shiny::h3("Manual table"),
                    shiny::br(),
                    DT::dataTableOutput("saved_manual_table"),
                    shiny::br(),
                    shiny::actionButton("reset_manual", "Reset table",
                                        icon = icon('eraser'))
                  ),

                  column(
                    2,
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

        # time frame table generator
        timeframe_gen <- reactive({
          sfndata <- sfndataInput()
          variable <- input$tree_env
          start_time <- lubridate::as_datetime(
            input$time_series_date_window[[1]],
            tz = attr(get_sapf_flags(sfndata)[['TIMESTAMP']], 'tz')
          )
          end_time <- lubridate::as_datetime(
            input$time_series_date_window[[2]],
            tz = attr(get_sapf_flags(sfndata)[['TIMESTAMP']], 'tz')
          )

          manual_tab <- get_sapf_flags(sfndata) %>%
            dplyr::full_join(get_env_flags(sfndata), by = 'TIMESTAMP') %>%
            dplyr::mutate(index = rownames(.)) %>%
            dplyr::filter(TIMESTAMP >= start_time & TIMESTAMP <= end_time) %>%
            dplyr::select(index) %>%
            dplyr::mutate(variable = variable)

          manual_tab
        })

        output$time_1 <- renderPrint({
          sfndata <- sfndataInput()
          print(lubridate::as_datetime(
            input$time_series_date_window[[1]],
            tz = lubridate::tz(get_timestamp(sfndata))
          ))
        })

        output$time_2 <- renderPrint({
          sfndata <- sfndataInput()
          print(lubridate::as_datetime(
            input$time_series_date_window[[2]],
            tz = lubridate::tz(get_timestamp(sfndata))
          ))
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
            dygraphs::dygraph('Time Series') %>%
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

        # write manual table when button is pressed
        observeEvent(
          eventExpr = input$timeframe,
          handlerExpr = {
            table_to_write <- timeframe_gen()
            file_name_out <- file.path(
              'Data', input$site_sel,
              'Lvl_2', 'lvl_2_out_warn',
              paste0(input$site_sel, '_manual_to_remove.txt')
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
        out_table_reader <- shiny::reactive({
          file_name <- file.path('Data', input$site_sel,
                                 'Lvl_2', 'lvl_2_out_warn',
                                 paste0(input$site_sel, '_out_to_remove.txt'))

          # inputs, only for redrawing the table purposes
          button_timeframe <- input$timeframe
          button_write_out <- input$write_out
          button_write_range <- input$write_range
          button_reset_out <- input$reset_out
          button_reset_range <- input$reset_range
          button_reset_manual <- input$reset_manual

          if (file.exists(file_name)) {
            read.table(file_name, header = TRUE)
          }
        })

        output$saved_out_table <- DT::renderDataTable({
          out_table_reader() %>%
            DT::datatable(extensions = 'Scroller',
                          options = list(dom = 't',
                                         deferRender = TRUE,
                                         scrollY = 300,
                                         scroller = TRUE))
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

        # Ranges to remove table
        range_table_reader <- shiny::reactive({
          file_name <- file.path('Data', input$site_sel,
                                 'Lvl_2', 'lvl_2_out_warn',
                                 paste0(input$site_sel, '_ranges_to_remove.txt'))

          # inputs, only for redrawing the table purposes
          button_timeframe <- input$timeframe
          button_write_out <- input$write_out
          button_write_range <- input$write_range
          button_reset_out <- input$reset_out
          button_reset_range <- input$reset_range
          button_reset_manual <- input$reset_manual

          if (file.exists(file_name)) {
            read.table(file_name, header = TRUE)
          }
        })

        output$saved_range_table <- DT::renderDataTable({
          range_table_reader() %>%
            DT::datatable(extensions = 'Scroller',
                          options = list(dom = 't',
                                         deferRender = TRUE,
                                         scrollY = 300,
                                         scroller = TRUE))
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

        # manual to remove table
        manual_table_reader <- shiny::reactive({
          file_name <- file.path('Data', input$site_sel,
                                'Lvl_2', 'lvl_2_out_warn',
                                paste0(input$site_sel, '_manual_to_remove.txt'))

          # inputs, only for redrawing the table purposes
          button_timeframe <- input$timeframe
          button_write_out <- input$write_out
          button_write_range <- input$write_range
          button_reset_out <- input$reset_out
          button_reset_range <- input$reset_range
          button_reset_manual <- input$reset_manual

          if (file.exists(file_name)) {
            read.table(file_name, header = TRUE)
          }
        })

        output$saved_manual_table <- DT::renderDataTable({
          manual_table_reader() %>%
            DT::datatable(extensions = 'Scroller',
                          options = list(dom = 't',
                                         deferRender = TRUE,
                                         scrollY = 300,
                                         scroller = TRUE))
        })

        # reset
        observeEvent(
          eventExpr = input$reset_manual,
          handlerExpr = {
            file_name_out <- file.path('Data', input$site_sel,
                                       'Lvl_2', 'lvl_2_out_warn',
                                       paste0(input$site_sel, '_manual_to_remove.txt'))

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

################################################################################
#' outlier confirmation app
#'
#' Interactive app to confirm the data after removing outliers, out of range
#' values and other modifications
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
out_confirmation_app <- function(parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Needed misc

    # 0.1 site codes list (only those with outliers removing done)
    site_list <- names(
      sapfluxnetQC1::df_whos_ready_to('rem', 'done',
                                      parent_logger = parent_logger)
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
        titlePanel('Out removing check'),

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
                    width = 10,
                    dygraphs::dygraphOutput("time_series", height = "250px")
                  ),

                  column(
                    2,
                    shiny::h3("Set status"),
                    shiny::br(),
                    shiny::actionButton("set_status", "Statutize!",
                                        icon = icon('gavel')),
                    shiny::br(),
                    shiny::br(),
                    shiny::br(),
                    shiny::actionButton("reset_status", "ARGHH!! GO BACK PLEASE!",
                                        icon = icon('snapchat-ghost')),
                    shiny::h4('Please, be advised that resetting the status of ',
                              'Level 2 means that site has to be unfreezed from ',
                              'Level 1 again ¯\\_(ツ)_/¯')
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
          df_read_SfnData(input$site_sel, 'out_rem')
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
            sapf_data[stringr::str_detect(sapf_flags[,i], 'OUT_REPLACED'), i] <- NA
            sapf_data[stringr::str_detect(sapf_flags[,i], 'RANGE_REMOVE'), i] <- NA
            sapf_data_out[!stringr::str_detect(sapf_flags[,i], 'OUT_REPLACED'), i] <- NA
            sapf_data_range[!stringr::str_detect(sapf_flags[,i], 'RANGE_REMOVE'), i] <- NA
          }

          for (i in 1:ncol(env_data)) {
            env_data[stringr::str_detect(env_flags[,i], 'OUT_REPLACED'), i] <- NA
            env_data[stringr::str_detect(env_flags[,i], 'RANGE_REMOVE'), i] <- NA
            env_data_out[!stringr::str_detect(env_flags[,i], 'OUT_REPLACED'), i] <- NA
            env_data_range[!stringr::str_detect(env_flags[,i], 'RANGE_REMOVE'), i] <- NA
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

        # Write status button
        observeEvent(
          eventExpr = input$set_status,
          handlerExpr = {
            df_set_status(input$site_sel, LVL2 = list(TO_UNITS = 'READY'))
          }
        )

        # Reset status button
        observeEvent(
          eventExpr = input$reset_status,
          handlerExpr = {
            df_reset_data_status(input$site_sel, 'LVL2')
          }
        )
      }
    )
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'out_confirmation_app',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'out_confirmation_app',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'out_confirmation_app',
                                                        sep = '.'))})
}
