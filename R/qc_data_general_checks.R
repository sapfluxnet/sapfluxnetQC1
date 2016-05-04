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
qc_is_timestamp <- function(data, verbose = TRUE) {

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
}

################################################################################
#' Fixing known errors in TIMESTAMP format
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
qc_as_timestamp <- function(data) {

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
qc_timestamp_errors <- function(data, timestep = 15) {

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
}

################################################################################
#' Gaps function (TO DO)
