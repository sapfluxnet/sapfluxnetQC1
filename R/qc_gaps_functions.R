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
#' Gaps are considered from the last TIMESTAMP with data to the next TIMESTAMP
#' with data. In this way, a gap of only one TIMESTAMP has a duration equal to
#' the timestep of the TIMESTAMP. The only exception to this is if the gap
#' includes the start or the end of the TIMESTAMP. In this case the gap start
#' or end in the first or the last TIMESTAMP
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


# START
# Function declaration
qc_mind_the_gap_deprecated <- function(data, trim = FALSE, parent_logger = 'test') {

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
        # dplyr::select_('TIMESTAMP', var)
        dplyr::select(dplyr::all_of(c('TIMESTAMP', var)))

      # 2.2 for each value
      for (i in 1:length(temp_data[[2]])) {
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
        if (is.na(temp_data[[i, 2]]) && i == length(temp_data[[2]])) {
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

    # STEP 2'5
    # Check if no gaps, as empty vectors will produce problems in the data
    # processing pipeline
    if (any(length(id) < 1, length(start) < 1, length(end) < 1)) {
      id <- 'No gaps found'
      start <- NA
      end <- NA
    }

    # STEP 3
    # Build the results data frame
    res_df <- data.frame(
      Object = id,
      gap_start = as.POSIXct(start, origin = lubridate::origin,
                             tz = attr(data$TIMESTAMP, 'tzone')),
      gap_end = as.POSIXct(end, origin = lubridate::origin,
                           tz = attr(data$TIMESTAMP, 'tzone')),
      stringsAsFactors = FALSE
    )

    timestamp_interval <- lubridate::int_length(
      lubridate::interval(
        data$TIMESTAMP[[1]],
        data$TIMESTAMP[[length(data$TIMESTAMP)]]
      )
    )/60

    res <- res_df %>%
      # interval in minutes
      dplyr::mutate(gap_interval = lubridate::int_length(lubridate::interval(gap_start, gap_end))/60,
                    # coverage percent
                    gap_coverage = (gap_interval / timestamp_interval),
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





#' Gaps info
#'
#' Information about gaps
#'
#' Gaps are considered from the last TIMESTAMP with data to the next TIMESTAMP
#' with data. In this way, a gap of only one TIMESTAMP has a duration equal to
#' the timestep of the TIMESTAMP. The only exception to this is if the gap
#' includes the start or the end of the TIMESTAMP. In this case the gap start
#' or end in the first or the last TIMESTAMP
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
    # Ensure TIMESTAMP is of class POSIXct
    if (!inherits(data$TIMESTAMP, "POSIXct")) {
      stop('TIMESTAMP variable must be of class POSIXct')
    }

    # STEP 1
    # Initialize result list
    res_list <- list()

    # Get the list of variables excluding TIMESTAMP
    vars <- setdiff(names(data), 'TIMESTAMP')

    # Total time interval in minutes
    timestamp_interval <- as.numeric(difftime(
      max(data$TIMESTAMP),
      min(data$TIMESTAMP),
      units = 'mins'
    ))

    # STEP 2
    # Vectorized operation using rle for each variable
    for (var in vars) {
      var_data <- data[[var]]
      is_na <- is.na(var_data)
      rle_na <- rle(is_na)
      lengths <- rle_na$lengths
      values <- rle_na$values
      ends <- cumsum(lengths)
      starts <- c(1, head(ends, -1) + 1)

      # Identify NA runs
      na_runs <- which(values == TRUE)
      if (length(na_runs) > 0) {
        gap_starts <- starts[na_runs]
        gap_ends <- ends[na_runs]

        # Build the results data frame
        res_df <- data.frame(
          Object = var,
          gap_start = data$TIMESTAMP[gap_starts],
          gap_end = data$TIMESTAMP[gap_ends],
          stringsAsFactors = FALSE
        )

        # Calculate intervals and coverage
        res_df$gap_interval <- as.numeric(difftime(
          res_df$gap_end,
          res_df$gap_start,
          units = 'min'
        ))
        res_df$gap_coverage <- res_df$gap_interval / timestamp_interval
        res_df$timestamp_start <- min(data$TIMESTAMP)
        res_df$timestamp_end <- max(data$TIMESTAMP)

        res_list[[var]] <- res_df
      }
    }

    # STEP 3
    # Combine all results
    if (length(res_list) > 0) {
      res <- do.call(rbind, res_list)
    } else {
      res <- data.frame(
        Object = 'No gaps found',
        gap_start = NA,
        gap_end = NA,
        gap_interval = NA,
        gap_coverage = NA,
        timestamp_start = min(data$TIMESTAMP),
        timestamp_end = max(data$TIMESTAMP),
        stringsAsFactors = FALSE
      )
    }

    # STEP 4
    # Trim if necessary
    if (trim) {
      res <- res[!(res$gap_start == res$timestamp_start &
                     res$gap_end == res$timestamp_end), ]
    }

    # Return the result
    return(res)

  },
  # handlers
  warning = function(w) {
    logging::logwarn(
      w$message,
      logger = paste(parent_logger, 'qc_mind_the_gap', sep = '.')
    )
  },
  error = function(e) {
    logging::logerror(
      e$message,
      logger = paste(parent_logger, 'qc_mind_the_gap', sep = '.')
    )
  },
  message = function(m) {
    logging::loginfo(
      m$message,
      logger = paste(parent_logger, 'qc_mind_the_gap', sep = '.')
    )
  })
}
