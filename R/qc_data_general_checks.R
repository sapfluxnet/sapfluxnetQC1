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

      # is all TIMESTAMP NAs?
      if (all(is.na(data$TIMESTAMP))) {
        if (verbose) {warning('WARNING: TIMESTAMP is all NAs')}
        return(invisible(FALSE))
      }

      # Check TIMESTAMP format
      if (lubridate::is.POSIXt(data$TIMESTAMP)) {
        if (verbose) {message('TIMESTAMP is in the correct format')}
        return(invisible(TRUE))
      } else {
        if (verbose) {warning('WARNING: TIMESTAMP is NOT in the correct format')}
        return(invisible(FALSE))
      }
    } else {

      # STEP 2
      # Vector
      # is all vector NA?
      if (all(is.na(data))) {
        if (verbose) {warning('WARNING: TIMESTAMP is all NAs')}
        return(invisible(FALSE))
      }

      if (lubridate::is.POSIXt(data)) {
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
                                         logger = paste(parent_logger,
                                                        'qc_is_timestamp', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_is_timestamp', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_is_timestamp', sep = '.'))})

}

################################################################################
#' Timezones dictionary
#'
#' Tranforms timezone ISO code to character vector compatible with lubridate and
#' POSIXct
#'
#' GMT time zones are used, as they are day saving light time (DST) agnostic,
#' and in that way the DST can setted if the metadata says so. GMT are sign
#' exchanged to be compatible with ISO.
#'
#' @family Quality Checks Functions
#'
#' @param tz Character vector with the ISO code of the timezone as provided in
#'   \code{env_time_zone} variable in \code{environmental_md}
#'
#' @return A character vector with the timezone code compatible with lubridate
#'   and as.POSIXct
#'
#' @export

# START
# Function declaration
qc_get_timezone <- function(tz, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Create the list with the codes
    timezones <- list(
      "1UTC-12:00, Y" = "Etc/GMT+12",
      "2UTC-11:00, X" = "Etc/GMT+11",
      "3UTC-10:00, W" = "Etc/GMT+10",
      "4UTC-09:30, V†" = "Pacific/Marquesas",
      "5UTC-09:00, V" = "Etc/GMT+9",
      "6UTC-08:00, U" = "Etc/GMT+8",
      "7UTC-07:00, T" = "Etc/GMT+7",
      "8UTC-06:00, S" = "Etc/GMT+6",
      "9UTC-05:00, R" = "Etc/GMT+5",
      "11UTC-04:00, Q" = "Etc/GMT+4",
      "12UTC-03:30, P†" = "Canada/Newfoundland",
      "13UTC-03:00, P" = "Etc/GMT+3",
      "14UTC-02:00, O" = "Etc/GMT+2",
      "15UTC-01:00, N" = "Etc/GMT+1",
      "16UTC±00:00, Z" = "Etc/GMT+0",
      "17UTC+01:00, A" = "Etc/GMT-1",
      "18UTC+02:00, B" = "Etc/GMT-2",
      "19UTC+03:00, C" = "Etc/GMT-3",
      "20UTC+03:30, C†" = "Asia/Tehran",
      "21UTC+04:00, D" = "Etc/GMT-4",
      "22UTC+04:30, D†" = "Asia/Kabul",
      "23UTC+05:00, E" = "Etc/GMT-5",
      "24UTC+05:30, E†" = "Asia/Kolkata",
      "25UTC+05:45, E*" = "Asia/Katmandu",
      "26UTC+06:00, F" = "Etc/GMT-6",
      "27UTC+06:30, F†" = "Indian/Cocos",
      "28UTC+07:00, G" = "Etc/GMT-7",
      "29UTC+08:00, H" = "Etc/GMT-8",
      "30UTC+08:30, H†" = "Asia/Pyongyang",
      "31UTC+08:45, H*" = "Australia/Eucla",
      "32UTC+09:00, I" = "Etc/GMT-9",
      "33UTC+09:30, I†" = "Australia/Adelaide",
      "34UTC+10:00, K" = "Etc/GMT-10",
      "35UTC+10:30, K†" = "Australia/Lord_Howe",
      "36UTC+11:00, L" = "Etc/GMT-11",
      "37UTC+12:00, M" = "Etc/GMT-12",
      "38UTC+12:45, M*" = "Pacific/Chatham",
      "39UTC+13:00, M†" = "Etc/GMT-13",
      "40UTC+14:00, M†" = "Etc/GMT-14"
    )

    # STEP 2
    # Return the timezone name compatible with lubridate
    return(timezones[[as.character(tz)]])

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_get_timezone', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_get_timezone', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_get_timezone', sep = '.'))})
}

################################################################################
#' Set the timezone of the TIMESTAMP
#'
#' Brute force convert of timezone
#'
#' When reading data from xlsx or csv, TIMESTAMP is readed as POSIXct and by
#' default the timezone is UTC. With this function timezone can be changed
#' without change the TIMESTAMP. This is made with the \code{force_tz} function
#' of the lubridate package.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame with the TIMESTAMP variable to set, or a POSIXct
#'   vector.
#'
#' @param tz Character vector with the compatible name of the timezone, as the
#'   one provided by the \code{\link{qc_get_timezone}} function.
#'
#' @return A data frame as the \code{data} provided, with the TIMESTAMP variable
#'   associated to the timezone specified
#'
#' @export

# START
# Function declaration
qc_set_timezone <- function(data, tz, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # is data a data frame?
    if (!is.data.frame(data) & !is.vector(data) & class(data)[1] != 'POSIXct') {
      stop('data is not a data frame or a POSIX vector')
    }

    # STEP 1
    # Data frame
    if (is.data.frame(data)) {

      # 1.1 has data a TIMESTAMP variable
      if (is.null(data$TIMESTAMP)) {
        stop('data has not a TIMESTAMP variable')
      }

      # 1.2 Force the timezone
      data$TIMESTAMP <- lubridate::force_tz(data$TIMESTAMP, tz)

      # 1.3 Return the data
      return(data)

    } else {
      # STEP 2
      # Vector

      # 2.1 force the timezone
      data <- lubridate::force_tz(data, tz)

      # 2.2 return the results
      return(data)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_set_timezone', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_set_timezone', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_set_timezone', sep = '.'))})


}

################################################################################
#' Convert known bad formats to correct TIMESTAMP format and set timezone
#'
#' Converting known bad TIMESTAMP formats to POSIXt and setting the correct
#' timezone
#'
#' When loading data from csv files, depending on the office version and
#' workflow to introduce TIMESTAMP and data, TIMESTAMP can result in the wrong
#' format (lost of seconds, for example). This function checks for known
#' formatting errors and try to fix them.
#'
#' @section Timezone:
#' This function also set the timezone attribute to the POSIXt TIMESTAMP.
#' It uses \code{\link{qc_get_timezone}} and \code{\link{qc_set_timezone}}
#' functions internally to get the timezone from the \code{env_time_zone}
#' variable, transforming it to a compatible timezone name and set it as a
#' POSIXt attribute.
#'
#' @family Data Loading Functions
#'
#' @param data Data frame containing TIMESTAMP variable. Also it can be a vector
#'   with TIMESTAMP values
#'
#' @param env_md Data frame containing the environmental metadata, in order
#'   to obtain the timezone information
#'
#' @return An object of the same type of input (data frame or vector) with the
#'   fixed values of TIMESTAMP. If TIMESTAMP is already in format, a message
#'   appears and none fix is made, returning data as entered.
#'   If TIMESTAMP can not be fixed, an error is raised.
#'
#' @export

# START
# Function declaration
qc_as_timestamp <- function(data, env_md, parent_logger = 'test') {

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
      timezone <- qc_get_timezone(env_md$env_time_zone,
                                  parent_logger = parent_logger)

      # 1.1 if already in format, inform and return the data unaltered
      if (qc_is_timestamp(data, verbose = FALSE,
                          parent_logger = parent_logger)) {

        # 1.1.1 Set the timezone
        res <- qc_set_timezone(data, timezone, parent_logger = parent_logger)

        message(paste('TIMESTAMP already in format. Timezone set to ',
                      timezone, sep = ''))
        return(res)
      } else {

        # 1.2 If not in format, try to fix it using the known bad formats
        res <- lubridate::parse_date_time(
          timestamp,
          c(# csv without seconds, in european and usa formats
            "%d%m%y %H:%M", "%y%m%d %H:%M",
            # csv with seconds, but / present, in european and usa formats
            "%d%m%y %H:%M:%S", "%y%m%d %H:%M:%S"
          ),
          tz = timezone
        )
      }

      # 1.3 Check if the fix worked. If yes, message and return data
      # with the new TIMESTAMP
      if (qc_is_timestamp(res, verbose = FALSE,
                          parent_logger = parent_logger)) {
        message('TIMESTAMP succesfully fixed. A sample: ', res[1])
        data$TIMESTAMP <- res
        return(data)
      } else {
        stop('Unable to format correctly the TIMESTAMP, please ',
              'revise manually.')
      }
    } else {

      # STEP 2
      # Vector
      # 2.1 If already in format, inform and return the data unaltered
      timezone <- qc_get_timezone(env_md$env_time_zone,
                                  parent_logger = parent_logger)

      if (qc_is_timestamp(data, verbose = FALSE)) {

        # 2.2 set the timezone
        res <- qc_set_timezone(data, timezone, parent_logger = parent_logger)

        message(paste('TIMESTAMP already in format. Timezone set to ',
                      timezone, sep = ''))
        return(res)
      } else {

        # 2.3 If not in format, try to fix it using the known bad formats
        res <- lubridate::parse_date_time(
          data,
          c(# csv without seconds, in european and usa formats
            "%d%m%y %H:%M", "%y%m%d %H:%M",
            # csv with seconds, but / present, in european and usa formats
            "%d%m%y %H:%M:%S", "%y%m%d %H:%M:%S"
          ),
          tz = timezone
        )
      }

      # 1.3 Check if the fix worked. If yes, message and return data
      # with the new TIMESTAMP
      if (qc_is_timestamp(res, verbose = FALSE, parent_logger = parent_logger)) {
        message('TIMESTAMP succesfully fixed. A sample: ', res[1])
        return(res)
      } else {
        stop('Unable to format correctly the TIMESTAMP, please ',
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
#' @param data Data frame containing the TIMESTAMP variable. Also it can be a
#'   vector with the TIMESTAMPS values.
#'
#' @param env_md Data frame containing the env_time_zone variable to retrieve
#'   the timezone name
#'
#' @return An object of the same type of input (data frame or vector) with the
#'   fixed values of TIMESTAMP. If TIMESTAMP is already in format, only timezone
#'   is set, returning data as entered.
#'   If TIMESTAMP can not be fixed, an error is raised.
#'
#' @export

# START
# Function declaration
qc_fix_timestamp <- function(data, env_md, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Check if TIMESTAMP is correct and set the timezone
    res <- qc_as_timestamp(data, env_md, parent_logger = parent_logger)

    # STEP 2
    # Return the result
    return(res)

    # if (qc_is_timestamp(data)) {
    #
    #   # 1.1 If correct, set the timezone
    #   return(qc_as_timestamp(data, env_md))
    # } else {
    #
    #   # STEP 2
    #   # If not correct, fix it
    #   res <- qc_as_timestamp(data, env_md)
    #
    #   # 2.1 and return it
    #   return(res)
    # }

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
#' Checking for NAs in the TIMESTAMP
#'
#' Simple function to check for NAs in the TIMESTAMP.
#'
#' NAs in TIMESTAMP generates problems in the further steps of QC, a function
#' checking for NAs and info about the location is needed in order to be
#' able to fix it
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the TIMESTAMP variable (sapflow or
#'   environmental data)
#'
#' @return A data frame with the NAs info
#'
#' @export

# START
# Function declaration
qc_timestamp_nas <- function(data, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(data)) {
      stop('Data provided is not a data frame')
    }
    if (is.null(data$TIMESTAMP)) {
      stop('Data provided has not a TIMESTAMP variable')
    }

    # STEP 1
    # Retrieving info about NAs in TIMESTAMP
    if (!any(is.na(data$TIMESTAMP))) {

      # 1.1 If no NAs, return TRUE
      return(invisible(TRUE))
    } else {

      # 1.2 If NAs, return the NAs
      res_df <- data %>%
        dplyr::mutate(row_number = row.names(data)) %>%
        dplyr::filter(is.na(TIMESTAMP))

      # STEP 2
      # Return the res_df
      return(res_df)
    }
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_timestamp_nas', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_timestamp_nas', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_timestamp_nas', sep = '.'))})
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
#' @param metadata Data frame containing the timestep variable (pl_sens_timestep
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
    if (!any(c('pl_sens_timestep', 'env_timestep') %in% names(metadata))) {
    # if (is.null(metadata$pl_sens_timestep) & is.null(metadata$env_timestep)) {
      stop('Not timestep variables found in metadata provided')
    }

    # STEP 1
    # Guess which variable is needed

    # 1.1 env_timestep
    if ('env_timestep' %in% names(metadata)) {
    # if (is.null(metadata$pl_sens_timestep) & !is.null(metadata$env_timestep)) {

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
#' Getting the \eqn{t_0} and \eqn{t_f} for trees or environmental data
#'
#' Summary of \eqn{t_0} and \eqn{t_f} for each tree or for each environmental
#' variable
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the sapflow values for each tree or the
#'   environmental variables values. A \code{TIMESTAMP} variable must be present
#'   in the dataset
#'
#' @return A data frame summarising the time intervals for each object(trees or
#'   environmental variables)
#'
#' @export

# START
# Function declaration
qc_time_interval <- function(data, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is data a data frame?
    if (!is.data.frame(data)) {
      stop('data provided is not a data frame')
    }
    # Is there a TIMESTAMP variable in data?
    if (is.null(data$TIMESTAMP)) {
      stop('data has not a TIMESTAMP variable')
    }

    # STEP 1
    # Initialise the empty results object
    res <- data.frame(
      Object = 'Total',
      t0 = data$TIMESTAMP[1],
      tf = data$TIMESTAMP[length(data$TIMESTAMP)],
      stringsAsFactors = FALSE
    )

    # STEP 2
    # For loop to iterate each object and obtain the t0 and the tf
    for (var in names(data)[-1]) {

      # 2.0 create a standard eval object to allow quoted vars in the filter
      #     step
      dots <- paste('!is.na(', var, ')', sep = '')

      res <- dplyr::bind_rows(
        res,
        {data %>%
            dplyr::select_('TIMESTAMP', var) %>%
            # 2.1 Filter to avoid NAs
            dplyr::filter_(.dots = dots) %>%
            # 2.2 Summarise to obtain the first and last value od TIMESTAMP
            dplyr::summarise(t0 = first(TIMESTAMP),
                             tf = last(TIMESTAMP)) %>%
            # 2.3 Add the object name
            dplyr::mutate(Object = var) %>%
            # 2.4 Reorder variables
            dplyr::select(Object, t0, tf)
        }
      )
    }

    # STEP 3
    # Return the res object
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_time_interval', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_time_interval', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_time_interval', sep = '.'))})
}

################################################################################
#' Check concordance between sapflow TIMESTAMP and env TIMESTAMP
#'
#' Are the sapflow and environmental TIMESTAMPS in concordance?
#'
#' This function uses \code{\link{qc_time_interval}} internally to perform
#' operations with time intervals. There is two ways of using this function:
#' \enumerate{
#'   \item Directly from sapflow and environmental data, providing both datasets
#'   \item From the results obtained from \code{\link{qc_time_interval}},
#'         providing both data frames with the results.
#' }
#'
#' @family Quality Checks Functions
#'
#' @param sapf_data Data frame containing the sapflow data and its TIMESTAMP
#'
#' @param env_data Data frame containing the environmental data and its
#'   TIMESTAMP
#'
#' @param sapf_intervals Data frame obtained from \code{\link{qc_time_interval}},
#'   optional. It can be used if no sapf and env data is provided
#'
#' @param env_intervals Data frame obtained from \code{\link{qc_time_interval}},
#'   optional. It can be used if no sapf and env data is provided
#'
#' @param plot Logical indicating if the result is presented in graphical mode
#'   (ggplot2 object). Desfault to TRUE.
#'
#' @return A data frame summarising the results
#'
#' @export

# START
# Function declaration
qc_timestamp_concordance <- function(sapf_data = NULL, env_data = NULL,
                                     sapf_intervals = NULL, env_intervals = NULL,
                                     plot = FALSE, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Arguments check
    # All NULL??
    if (all(is.null(sapf_data), is.null(env_data),
            is.null(sapf_intervals), is.null(env_intervals))) {
      stop('No data provided')
    }

    # STEP 1
    # Raw data or intervals data?

    # 1.1 Raw data
    if (all(!is.null(sapf_data), !is.null(env_data))) {

      # 1.1.1 generate the intervals
      sapf_intervals <- qc_time_interval(sapf_data, parent_logger = parent_logger)
      env_intervals <- qc_time_interval(env_data, parent_logger = parent_logger)
    }

    # 1.2 Intervals data (now all are intervals)
    sapf_intervals$Object[1] <- 'Total_sapf'
    env_intervals$Object[1] <- 'Total_env'
    intervals_data <- dplyr::bind_rows(sapf_intervals, env_intervals)

    # STEP 2
    # Plot?
    if (plot) {
      intervals_plot <- intervals_data %>%
        tidyr::gather(Time_point, Value, -Object) %>%
        dplyr::mutate(Object = factor(Object, levels = rev(unique(Object)))) %>%
        dplyr::group_by(Object, Time_point) %>%
        ggplot(aes(x = Value, y = Object, colour = Object)) +
        geom_line(size = 2) +
        scale_colour_manual(values = c(rep('darkgreen',
                                           length(env_intervals$Object)),
                                       rep('steelblue',
                                           length(sapf_intervals$Object)))) +
        scale_x_datetime(date_breaks = '1 month') +
        theme(legend.position = 'none')

      # 2.1 return the plot
      return(intervals_plot)

      # 2.2 No plot
    } else {

      # 2.2.1 return the info data frame
      return(intervals_data)
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_timestamp_concordance',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_timestamp_concordance',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_timestamp_concordance',
                                                        sep = '.'))})
}

################################################################################
#' Solar time conversion
#'
#' Calculate the Mean Solar Time and Apparent (Real) Solar Time from the
#' TIMESTAMP
#'
#' In order to obtain the mean solar time and the equation of time for each day
#' included in the TIMESTAMP functions from \code{solaR} package.
#'
#' @section Apparent (Real) Solar Time:
#' The Apparent Solar Time is calculated as:
#' \deqn{Apparent Solar Time = Mean Solar Time - Equation of Time}
#' The Equation of Time is calculated for each day, whereas the Mean Solar Time
#' is calculated for each step of the TIMESTAMP.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the TIMESTAMP variable to convert to solar
#'   time
#' @param site_md Data frame containing the latitude and longitude variables of
#'   the site (\code{si_lat} and \code{si_long})
#'
#' @param env_md Data frame containing the tz variable (\code{env_time_zone})
#'
#' @param type Character indicating which kind of solar time is desired,
#'   \code{mean} or \code{apparent}.
#'
#' @return A data frame exactly as \code{data}, but with the TIMESTAMP converted
#'   to the indicated solar time.
#'
#' @export

# START
# Function declaration
qc_solar_timestamp <- function(data, site_md, env_md, type = 'apparent',
                               parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

      # STEP 0
      # Argument checks
      # Are data, site_md and env_md data frames?
      if(any(!is.data.frame(data), !is.data.frame(site_md), !is.data.frame(env_md))) {
        stop('data, site_md and/or env_md are not data frames')
      }
      # have data the timestamp variable?
      if(is.null(data$TIMESTAMP)) {
        stop('data has not a TIMESTAMP variable')
      }
      # have metadata objects the mandatory variables?
      if(any(is.null(site_md$si_lat), is.null(site_md$si_long),
             is.null(env_md$env_time_zone))) {
        stop('metadata objects have not the needed variables. ',
             'See function help (?qc_solar_timestamp)')
      }
      # Is type a valid value?
      if (!(type %in% c('mean', 'apparent'))) {
        stop('type = "', type, '" is not a valid value. See function ',
             'help (?qc_solar_timestamp) for a list of valid values')
      }

      # STEP 1
      # Retrieve the accessory info
      tz <- qc_get_timezone(env_md$env_time_zone)
      lat <- site_md$si_lat
      long <- site_md$si_long
      timestamp <- data$TIMESTAMP

      # STEP 2
      # Intermediate objects
      # 2.2 Mean Solar Time
      mst <- solaR::local2Solar(timestamp, long)

      if (type == 'mean'){

        data$TIMESTAMP <- mst

      } else if (type == 'apparent'){

        # STEP 3
        # Calculating Apparent Solar Time (Mean Solar Time + Equation of Time)
        # 2.1 Equation of time
        solD <- solaR::fSolD(lat, timestamp)
        EoT <- solaR::r2sec(solD$EoT)

        ast <- sapply(as.Date(index(EoT)),
                      function(id,vect)
                        (vect[as.Date(vect) == id] + coredata(EoT)[which(as.Date(index(EoT)) == id)]),
                      vect = mst)

        ast <- do.call("c", ast)

        data$TIMESTAMP <- ast

      }

      return(data)

      # END FUNCTION
    },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_solar_timestamp',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_solar_timestamp',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_solar_timestamp',
                                                        sep = '.'))})
}
