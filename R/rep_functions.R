################################################################################
# Functions for automatic reporting                                            #
# Functions needed for render and customize automatic reports                  #
################################################################################
#' Render the report
#'
#' Wrap around rmarkdown::render function to generate the automatic reports
#'
#' @family Report Functions
#'
#' @param template Character vector indicating the name of the template. It must
#'   be located in the Templates folder of the project
#'
#' @param output_file Character vector indicating the name of the report file/s
#'
#' @param output_dir Character vector indicating the output folder
#'
#' @param ... Parameters to pass on the yaml preamble of Rmd template
#'
#' @return Nothing, report is generated in the Report folder (default) or the
#'   specified Report subfolder
#'
#' @export

# START
# Function declaration
rep_sfn_render <- function(template, output_file = NULL,
                           output_dir = NULL, parent_logger = 'test',
                           ...) {

  # Use calling handlers to logging to files
  withCallingHandlers({
    rmarkdown::render(input = file.path('Templates', template),
                      output_format = c('html_document'),
                      output_file = output_file,
                      output_dir = output_dir,
                      runtime = 'auto',
                      clean = TRUE,
                      params = list(...),
                      run_pandoc = TRUE,
                      quiet = TRUE)
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'rep_sfn_render', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'rep_sfn_render', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'rep_sfn_render', sep = '.'))})

}
