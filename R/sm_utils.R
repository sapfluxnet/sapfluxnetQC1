################################################################################
# SERVER MANAGEMENT UTILITIES                                                  #
################################################################################

#' Status updater to server management
#'
#' Quick update for status files to update them to the new sapfluxnetQC1 library
#' versions
#'
#' This function get the old status, save it, destroy the file, generate a new
#' one and use the old to update the new one. Use it to generate updated
#' status files for the sites after updating the QC package
#'
#' @family Server Management
#'
#' @param si_code character with the site code
#'
#' @return Invisible TRUE if no problem was found. Invisible FALSE if the site
#'   was not updated.
#'
#' @export

# START FUNCTION
# Function declaration
sm_status_updater <- function(si_code, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.character(si_code)) {
      stop('Site code argument is not a character')
    }

    # STEP 1
    # 1.1 Get the old status
    old_status <- df_get_status(si_code, parent_logger = parent_logger)

    if (is.logical(old_status)) {
      message(si_code, ' status file does not exist, skipping')
      return(invisible(FALSE))
    }

    # 1.2 remove the status file
    file.rename(
      from = file.path('Data', si_code, paste0(si_code, '_status.yaml')),
      to = file.path('Data', si_code, paste0(si_code, '_status.bak'))
    )

    # 1.3 create an empty status file
    df_start_status(si_code, parent_logger = parent_logger)

    # 1.4 update the newly created status with the old one
    df_set_status(
      si_code,
      QC = old_status[['QC']],
      LVL1 = old_status[['LVL1']],
      LVL2 = old_status[['LVL2']],
      parent_logger = parent_logger
    )

    # STEP 2
    # Check file creation
    if (!file.exists(file.path('Data', si_code, paste0(si_code, '_status.yaml')))) {
      warning('file was not created, backup saved')
      return(invisible(FALSE))
    } else {
      unlink(file.path('Data', si_code, paste0(si_code, '_status.bak')))
      return(invisible(TRUE))
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'df_status_updater',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'df_status_updater',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'df_status_updater',
                                                        sep = '.'))})
}
