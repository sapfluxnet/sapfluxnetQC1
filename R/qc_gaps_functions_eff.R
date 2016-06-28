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
qc_mind_the_gap_eff <- function(data, trim = FALSE, parent_logger = 'test') {

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
    # apply approach
    var_names <- names(data[,-1])

    # internal function to do with each variable
    gap_points_func <- function(var) {
      # temp data for each variable
      temp_data <- data %>%
        dplyr::select_('TIMESTAMP', var)

      # start value (with vapply)
      start_vec <- vapply(1:length(temp_data[[2]]), function(i) {
        # if value is the first value and is na, gap start point
        if (is.na(temp_data[[i, 2]]) && i == 1) {
          return(temp_data$TIMESTAMP[i])
        } else {
          # if value is na and the previous one is not, gap start point
          if (is.na(temp_data[[i, 2]]) && !is.na(temp_data[[i-1, 2]])) {
            return(temp_data$TIMESTAMP[i-1])
          } else { return(NA) }
        }
      }, vector('double', 1))

      # end value (with vapply)
      end_vec <- vapply(1:length(temp_data[[2]]), function(i) {
        # if value is na and the last one, gap end point
        if (is.na(temp_data[[i, 2]]) && i == length(temp_data[[2]])) {
          return(temp_data$TIMESTAMP[i])
        } else {
          # if value is na and the next one is not, gap end point
          if (is.na(temp_data[[i, 2]]) && !is.na(temp_data[[i+1, 2]])) {
            return(temp_data$TIMESTAMP[i+1])
          } else { return(NA) }
        }
      }, vector('double', 1))

      # strip NAs and create id_vec
      start_vec <- na.omit(start_vec)
      end_vec <- na.omit(end_vec)
      id_vec <- rep(var, length(end_vec))

      # data frame with results
      res_tmp <- data.frame(Object = id_vec,
                            gap_start = as.POSIXct(start_vec,
                                                   origin = lubridate::origin),
                            gap_end = as.POSIXct(end_vec,
                                                 origin = lubridate::origin))
      # return it
      return(res_tmp)
    }

    # Now, iterate variables and use the gap_points_funct
    res_list <- lapply(var_names, gap_points_func)

    res_df <- data.frame(Object = vector(),
                         gap_start = vector(),
                         gap_end = vector())

    for (df in res_list) {
      res_df <- dplyr::bind_rows(res_df, df)
    }

    timestamp_interval <- lubridate::int_length(lubridate::interval(data$TIMESTAMP[[1]],
                                                                    data$TIMESTAMP[[length(data$TIMESTAMP)]]))/60

    res <- res_df %>%
      # interval in minutes
      dplyr::mutate(gap_interval = lubridate::int_length(lubridate::interval(gap_start, gap_end))/60,
                    # coverage percent
                    gap_coverage = (gap_interval / timestamp_interval),
                    timestamp_start = data$TIMESTAMP[[1]],
                    timestamp_end = data$TIMESTAMP[[length(data$TIMESTAMP)]])

    # STEP 2
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
