################################################################################
#' TimeStamp format check
#'
#' Function to check if the format of the timestamp is the correct one
#'
#' Template timestamp is requiered to comply POSIXct format. This function
#' checks if the contributor followed the mandatory format.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the data (sapflow or environmental). Also
#'   a vector with the TIMESTAMP values. If data frame, \bold{it must contain} a
#'   TIMESTAMP variable
#'
#' @param verbose Logical indicating if messages of success and warnings of
#'   failures must be presented. Default is \code{TRUE}. In order to use
#'   inside another function, is recommended to set \code{verbose = FALSE}.
#'
#' @return A message/warning indicating if TIMESTAMP is correct or not. Also
#'   an invisible logical object is returned, indicating success (TRUE) or
#'   failure (FALSE).
#'
#' @export

# START
# Function declaration
qc_is_timestamp <- function(data, verbose = TRUE,
                            parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checking
    # is data a data frame?
    if(!is.data.frame(data) & !is.vector(data) & class(data)[1] != 'POSIXct') {
      stop('Data provided is not a data frame or a vector')
    }

    # STEP 1
    # Data frame
    if (is.data.frame(data)) {
      # have data a TIMESTAMP variable?
      if(is.null(data$TIMESTAMP)) {
        stop('TIMESTAMP variable is missing in the data provided')
      }
      # Check TIMESTAMP format
      if(lubridate::is.POSIXt(data$TIMESTAMP)) {
        if (verbose) {message('TIMESTAMP is in the correct format')}
        return(invisible(TRUE))
      } else {
        if (verbose) {warning('WARNING: TIMESTAMP is NOT in the correct format')}
        return(invisible(FALSE))
      }
    } else {

      # STEP 2
      # Vector
      if(lubridate::is.POSIXt(data)) {
        if (verbose) {message('TIMESTAMP is in the correct format')}
        return(invisible(TRUE))
      } else {
        if (verbose) {warning('WARNING: TIMESTAMP is NOT in the correct format')}
        return(invisible(FALSE))
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_is_timestamp', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_is_timestamp', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_is_timestamp', sep = '.'))})

}

################################################################################
#' Convert known bad formats to correct TIMESTAMP format
#'
#' Converting known bad TIMESTAMP formats to POSIXt
#'
#' When loading data from csv files, depending on the office version and
#' workflow to introduce TIMESTAMP and data, TIMESTAMP can result in the wrong
#' format (lost of seconds, for example). This function checks for known
#' formatting errors and try to fix them.
#'
#' @family Data Loading Functions
#'
#' @param data Data frame containing TIMESTAMP variable. Also it can be a vector
#'   with TIMESTAMP values
#'
#' @return An object of the same type of input (data frame or vector) with the
#'   fixed values of TIMESTAMP. If TIMESTAMP is already in format, a message
#'   appears and none fix is made, returning data as entered.
#'   If TIMESTAMP can not be fixed, an error is raised.
#'
#' @export

# START
# Function declaration
qc_as_timestamp <- function(data, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checking
    # Data is a vector or a data frame
    if (!is.data.frame(data) & !is.vector(data) & class(data)[1] != 'POSIXct') {
      stop('Data is not a data frame or a vector')
    }

    # STEP 1
    # Data frame
    if (is.data.frame(data)) {
      # Data contains a TIMESTAMP variable?
      if (is.null(data$TIMESTAMP)) {
        stop('Data have no TIMESTAMP variable')
      }
      timestamp <- data$TIMESTAMP

      # 1.1 if already in format, inform and return the data unaltered
      if (qc_is_timestamp(timestamp, verbose = FALSE)) {
        message('TIMESTAMP is already in format')
        return(data)
      } else {

        # 1.2 If not in format, try to fix it using the known bad formats
        res <- lubridate::parse_date_time(
          timestamp,
          c(# csv without seconds, in european and usa formats
            "%d%m%y %H:%M", "%y%m%d %H:%M",
            # csv with seconds, but / present, in european and usa formats
            "%d%m%y %H:%M:%S", "%y%m%d %H:%M:%S"
          )
        )
      }

      # 1.3 Check if the fix worked. If yes, message and return data
      # with the new TIMESTAMP
      if (qc_is_timestamp(res, verbose = FALSE)) {
        message('TIMESTAMP succesfully fixed. A sample: ', res[1])
        data$TIMESTAMP <- res
        return(data)
      } else {
        error('Unable to format correctly the TIMESTAMP, please ',
              'revise manually.')
      }
    } else {

      # STEP 2
      # Vector
      # 2.1 If already in format, inform and return the data unaltered
      if (qc_is_timestamp(data, verbose = FALSE)) {
        message('TIMESTAMP is already in format')
        return(data)
      } else {

        # 1.2 If not in format, try to fix it using the known bad formats
        res <- lubridate::parse_date_time(
          data,
          c(# csv without seconds, in european and usa formats
            "%d%m%y %H:%M", "%y%m%d %H:%M",
            # csv with seconds, but / present, in european and usa formats
            "%d%m%y %H:%M:%S", "%y%m%d %H:%M:%S"
          )
        )
      }

      # 1.3 Check if the fix worked. If yes, message and return data
      # with the new TIMESTAMP
      if (qc_is_timestamp(res, verbose = FALSE)) {
        message('TIMESTAMP succesfully fixed. A sample: ', res[1])
        return(res)
      } else {
        error('Unable to format correctly the TIMESTAMP, please ',
              'revise manually.')
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_as_timestamp', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_as_timestamp', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_as_timestamp', sep = '.'))})

}

################################################################################
#' Fix TIMESTAMP formats
#'
#' Wrapper for \code{\link{qc_is_timestamp}} and \code{\link{qc_as_timestamp}}
#'
#' This function uses \code{\link{qc_is_timestamp}} and
#' \code{\link{qc_as_timestamp}} internally to check if the format is correct
#' and if not, try one of the known fixes to convert it in the correct
#' TIMESTAMP
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame contining the TIMESTAMP variable. Also it can be a
#'   vector with the TIMESTAMPS values.
#'
#' @return An object of the same type of input (data frame or vector) with the
#'   fixed values of TIMESTAMP. If TIMESTAMP is already in format, a message
#'   appears and none fix is made, returning data as entered.
#'   If TIMESTAMP can not be fixed, an error is raised.
#'
#' @export

# START
# Function declaration
qc_fix_timestamp <- function(data, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Check if TIMESTAMP is correct
    if (qc_is_timestamp(data)) {

      # 1.1 If correct, return the data without modifications
      return(data)
    } else {

      # STEP 2
      # If not correct, fix it
      res <- qc_as_timestamp(data)

      # 2.1 and return it
      return(res)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_fix_timestamp', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_fix_timestamp', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_fix_timestamp', sep = '.'))})
}

################################################################################
#' Helper function to get metadata timestep (environmental and sapflow).
#'
#' Get timestep value
#'
#' \code{\link{qc_timestamp_errors}} needs a value of timestep to be able to
#' identify deviations from the expected interval of time. This function can
#' extract the timestep for environmental or sapflow data.
#'
#' @family Quality Checks Functions
#'
#' @param metadata Data frame contining the timestep variable (pl_sens_timestep
#'   or env_timestep)
#'
#' @return The time step value as numeric
#'
#' @export

# START
# Function declaration
qc_get_timestep <- function(metadata, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # Helper function to check if all elements of a vector are equal, borrowed
    # from H. Wickham in
    # http://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
    zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
      # If length of the vector is 1, results is TRUE
      if (length(x) == 1) {
        return(TRUE)
      }
      # If not,
      x <- range(x) / mean(x)
      isTRUE(all.equal(x[1], x[2], tolerance = tol))
    }

    # STEP 0
    # Argument checks
    # is metadata a data frame?
    if (!is.data.frame(metadata)) {
      stop('metadata provided is not a data frame')
    }
    # we need pl_sens_timestemp or env_timestep variables
    if (is.null(metadata$pl_sens_timestep) & is.null(metadata$env_timestep)) {
      stop('Not timestep variables found in metadata provided')
    }

    # STEP 1
    # Guess which variable is needed

    # 1.1 env_timestep
    if (is.null(metadata$pl_sens_timestep) & !is.null(metadata$env_timestep)) {

      # check if all timestep values (in the case of pl_sens_timestep) are the same
      timestep <- metadata$env_timestep
      if (!zero_range(timestep)) {
        stop('There are diferent timesteps in the metadata, please check manually')
      } else {
        return(timestep[[1]])
      }
    } else {

      # check if all timestep values (in the case of pl_sens_timestep) are the same
      timestep <- metadata$pl_sens_timestep
      if (!zero_range(timestep)) {
        stop('There are diferent timesteps in the metadata, please check manually')
      } else {
        return(timestep[[1]])
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_get_timestep', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_get_timestep', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_get_timestep', sep = '.'))})
}

################################################################################
#' TimeStamp errors localization
#'
#' Function to pinpoint errors in the timestamp
#'
#' \code{TIMESTAMP} variable can present continuity, date or another kind of
#' errors. This function checks for them. For that, from the timestep declared
#' in the metadata, a summary of intervals differing from it is
#' presented. Only intervals differing more than 59 seconds from the declared
#' timestep are reported.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the data (sapflow or environmental). It
#'   must have a \code{TIMESTAMP} variable.
#'
#' @param timestep Integer indicating the measures timestep (in minutes).
#'
#' @return A data frame summarizing the errors found, indicating the interval
#'   and its duration (in seconds).
#'
#' @export

# START
# Function declaration
qc_timestamp_errors <- function(data, timestep = 15,
                                parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # is data a data frame?
    if(!is.data.frame(data)) {
      stop('Data provided is not a data frame')
    }
    # have data a TIMESTAMP variable?
    if(is.null(data$TIMESTAMP)) {
      stop('TIMESTAMP variable is missing in the data provided')
    }
    # is timestep numeric?
    if(!is.numeric(timestep)) {
      stop('Provided timestep is not numeric')
    }

    # STEP 1
    # Initiate required values
    # 1.1 length in seconds of the expected interval
    length_seconds <- timestep * 60

    # STEP 2
    # Create the results object
    res <- dplyr::data_frame(Interval = lubridate::int_diff(data$TIMESTAMP),
                             Int_length = lubridate::int_length(Interval)) %>%
      # step neede to maintain the interval format
      dplyr::mutate(Interval = as.character(Interval)) %>%
      # drop the length values equal to the timestep plus/minus 59 seconds
      dplyr::filter(Int_length > (length_seconds + 59) | Int_length < (length_seconds - 59))

    # STEP 3
    # Return the results
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_timestamp_errors', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_timestamp_errors', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_timestamp_errors', sep = '.'))})

}

################################################################################
#' Gaps function (TO DO)
