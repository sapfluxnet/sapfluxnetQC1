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
#' @param data Data frame containing the data (sapflow or environmental). It
#'   must have a \code{TIMESTAMP} variable.
#'
#' @return A message indicating if TIMESTAMP is correct or not
#'
#' @import lubridate
#'
#' @export

# START
# Function declaration
qc_is_timestamp <- function(data) {

  # STEP 0
  # Argument checking
  # is data a data frame?
  if(!is.data.frame(data)) {
    stop('Data provided is not a data frame')
  }
  # have data a TIMESTAMP variable?
  if(is.null(data$TIMESTAMP)) {
    stop('TIMESTAMP variable is missing in the data provided')
  }

  # STEP 1
  # Check TIMESTAMP format
  if(lubridate::is.POSIXt(data$TIMESTAMP)) {
    message('TIMESTAMP is in the correct format')
  } else {
    warning('WARNING: TIMESTAMP is NOT in the correct format')
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
#' @import lubridate
#'
#' @import dplyr
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
