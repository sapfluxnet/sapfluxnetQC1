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

# START
# Function declaration
out_app <- function() {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
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
