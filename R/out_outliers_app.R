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
out_app <- function() {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Needed misc

    # 0.1 site codes list
    site_list <- list.dirs('Data', full.names = FALSE, recursive = FALSE)

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
            uiOutput('tree_controls'),
            uiOutput('env_controls'),
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
                    dygraphOutput("time_series", height = "250px")
                  ),

                  # row with table and selectors
                  fluidRow(

                    # out table column
                    column(
                      width = 6,
                      tableOutput("out_table")
                    ),

                    # selectors column
                    column(
                      width = 6#,
                      #?????
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
      )
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
