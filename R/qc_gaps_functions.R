################################################################################
# GAPS FUNCTIONS                                                               #
#                                                                              #
# Functions to obtain info about gaps in data                                  #
################################################################################

################################################################################
#' Gaps info
#'
#' Information about gaps
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the data in which obtain info about gaps.
#'   It must have a TIMESTAMP variable
#'
#' @param trim Logial indicating if starting and ending gaps must be included.
#'   Default to FALSE.
#'
#' @return A data frame with info about gaps:
#'   \itemize{
#'       \item gap_start: Gap start TIMESTAMP
#'       \item gap_end: Gap end TIMESTAMP
#'       \item gap_interval: Gap interval in minutes
#'       \item gap_coverage: Gap coverage in percentage
#'       \item timestamp_start: Absolute timestamp start
#'       \item timestamp_end: Absolute timestamp end
#'   }
#'
#' @export

# START
# Function declaration
qc_mind_the_gap <- function(data, trim = FALSE, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is data a data.frame?
    if (!is.data.frame(data)) {
      stop('Data is not a data frame')
    }
    # Is there a TIMESTAMP variable?
    if (is.null(data$TIMESTAMP)) {
      stop('TIMESTAMP variable is missing in data')
    }

    # STEP 1
    # names and temp objects
    start <- vector()
    end <- vector()
    id <- vector()

    # STEP 2
    # For loop and nested loops

    # 2.1 for each variable
    for (var in names(data[,-1])) {
      temp_data <- data %>%
        dplyr::select_('TIMESTAMP', var)

      # 2.2 for each value
      for (i in 1:length(temp_data[,2])) {
        # start value
        # if value is the first value and is na, gap start point
        if (is.na(temp_data[[i, 2]]) && i == 1) {
          start <- c(start, temp_data$TIMESTAMP[i])
        } else {
          # if value is na and the previous one is not, gap start point
          if (is.na(temp_data[[i, 2]]) && !is.na(temp_data[[i-1, 2]])) {
            start <- c(start, temp_data$TIMESTAMP[i-1])
          }
        }
        # end value and id
        # if value is na and the last one, gap end point
        if (is.na(temp_data[[i, 2]]) && i == length(temp_data[,2])) {
          end <- c(end, temp_data$TIMESTAMP[i])
          id <- c(id, var)
        } else {
          # if value is na and the next one is not, gap end point
          if (is.na(temp_data[[i, 2]]) && !is.na(temp_data[[i+1, 2]])) {
            end <- c(end, temp_data$TIMESTAMP[i+1])
            id <- c(id, var)
          }
        }

      }
    }

    # STEP 3
    # Build the results data frame
    res_df <- data.frame(
      Id = id,
      gap_start = as.POSIXct(start, origin = lubridate::origin),
      gap_end = as.POSIXct(end, origin = lubridate::origin)
    )

    timestamp_interval <- lubridate::int_length(lubridate::interval(data$TIMESTAMP[[1]],
                                                                    data$TIMESTAMP[[length(data$TIMESTAMP)]]))/60

    res <- res_df %>%
      # interval in minutes
      dplyr::mutate(gap_interval = lubridate::int_length(lubridate::interval(gap_start, gap_end))/60,
                    # coverage percent
                    gap_coverage = (gap_interval / timestamp_interval)*100,
                    timestamp_start = data$TIMESTAMP[[1]],
                    timestamp_end = data$TIMESTAMP[[length(data$TIMESTAMP)]])

    # STEP 4
    # Return the res

    # 4.1 trim = FALSE
    if (!trim) {
      return(res)
    } else {
      # 4.2 trim = TRUE
      res_trimmed <- res %>%
        dplyr::filter(gap_start != timestamp_start & gap_end != timestamp_end)

      return(res_trimmed)
    }
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_mind_the_gap', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_mind_the_gap', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_mind_the_gap', sep = '.'))})
}
