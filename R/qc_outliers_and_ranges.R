################################################################################
#' Outliers substitution
#'
#' Outlier substitution by different methods
#'
#' \code{k} is the semi-value of the desired window for substitute value
#' calculation. The window is formed by the range \code{y[i] - k : y[i] + k}.
#'
#' @section out_tukeyline:
#'   \code{out_tukeyline} performs a robust fitting with the \code{line} function
#'
#' @section out_medianreg:
#'   \code{out_medianreg} performs a regression of medians with the quantreg package
#'
#' @section out_median:
#'   \code{out_median} performs a classic median outlier detection
#'
#' @family Quality Checks Functions
#'
#' @param y vector of values for outlier substitution
#'
#' @param k window semi-value (integer) for substitution value calculation. See
#'   details.
#'
#' @return a vector of the same lengh of y with the outlier values substituted
#'   by the calculated value
#'
#' @name qc_outliers_subs
NULL

################################################################################
#' @describeIn qc_outliers_subs
#'
#' @export

# START
# Function declaration
qc_out_tukeyline <- function(y, k = 25L, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 0
    # Argument checks
    # y numeric
    if (!is.numeric(y)) {
      stop('Vector of values provided is not numeric')
    }
    # k integer
    if (!is.integer(k)) {
      stop('Window value is not an integer')
    }

    # STEP 1
    # Initiate needed vectors and values
    m <- length(y)
    x <- -k:k
    r <- vector('numeric', m)

    # STEP 2
    # Iteration loop to calculate outliers substitutions
    for (i in (k + 1):(m - k)) {
      z <- y[(i - k):(i + k)]
      r[i] <- ifelse(sum(!is.na(z)) > 2,
                     line(x = x,y = z)$fitted.values[k + 1],
                     NA)
    }

    # STEP 3
    # Return the res vector
    return(r)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'out_tukeyline', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'out_tukeyline', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'out_tukeyline', sep = '.'))})
}

################################################################################
#' @describeIn qc_outliers_subs
#'
#' @export

# START
# Function declaration
qc_out_medianreg <- function(y, k = 25L, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 0
    # Argument checks
    # y numeric
    if (!is.numeric(y)) {
      stop('Vector of values provided is not numeric')
    }
    # k integer
    if (!is.integer(k)) {
      stop('Window value is not an integer')
    }

    # STEP 1
    # Initiate needed vectors and values
    m <- length(y)
    x <- -k:k
    r <- vector('numeric', m)

    # STEP 2
    # Iteration loop to calculate outliers substitutions
    for (i in (k+1):(m-k)) {
      z <- y[(i - k):(i + k)]
      r[i] <- ifelse(sum(!is.na(z)) > 5,
                     quantreg::rq(z ~ x, .5)$fitted.values[k + 1],
                     NA)
    }

    # STEP 3
    # Return the res vector
    return(r)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'out_medianreg', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'out_medianreg', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'out_medianreg', sep = '.'))})
}

################################################################################
#' @describeIn qc_outliers_subs
#'
#' @export

# START
# Function declaration
qc_out_median <- function(y, k = 25L, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({
    # STEP 0
    # Argument checks
    # y numeric
    if (!is.numeric(y)) {
      stop('Vector of values provided is not numeric')
    }
    # k integer
    if (!is.integer(k)) {
      stop('Window value is not an integer')
    }

    # STEP 1
    # Initiate needed objects
    n <- length(y)

    # STEP 2
    # results
    return(c(
      y[1:k],
      sapply((k + 1):(n - k), function(j) median(y[(j - k):(j + k)],
                                                 na.rm = TRUE)),
      y[(n-k+1):n]
    ))

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'out_median', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'out_median', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'out_median', sep = '.'))})
}

################################################################################
#' Hampel filter
#'
#' Hampel filter for detecting and substituting outliers in the environmental
#' and sap flow data.
#'
#' This is a modified version of the Hampel filter to avoid breaking when NAs
#' are present in the data. Options include the posibility of using the
#' median (classic Hampel, default), tukeyline or quantile regression estimations.
#'
#' @family Quality Checks Functions
#'
#' @param y vector of values for outlier substitution
#'
#' @param k window semi-value (integer) for substitution value calculation. See
#'   details.
#'
#' @param t0 Integer value indicating the number of standard deviations for
#'   outlier detection threshold.
#'
#' @param method Character vector indicating the method to use in the estimation:
#'   \code{hampel} (default), \code{tukey} and \code{quantile}.
#'
#' @param reverse Logical indicating if in case of k is bigger than the data
#'   length (i.e in the extremes of the vector) a reverse replication must be
#'   done. Default to TRUE.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item{res: A vector of the same length as y with the ouliers value
#'     substituted by the estimation}
#'     \item{index: A vector indicating which TIMESTAMPS have been modified}
#'   }
#'
#' @export

# START
# Function declaration
qc_out_hampel_filter <- function(y, k = 25L, t0 = 10L,
                              method = 'hampel', reverse = TRUE,
                              parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # y numeric
    if (!is.numeric(y)) {
      stop('Vector of values provided is not numeric')
    }
    # k integer
    if (!is.integer(k)) {
      stop('Window value is not an integer')
    }
    # t0 integer
    if (!is.integer(t0)) {
      stop('T0 value provided is not an integer')
    }
    # reverse logical
    if (!is.logical(reverse)) {
      stop('reverse parameter must be TRUE or FALSE')
    }

    # STEP 1
    # Initialising needed objects
    n <- length(y)

    # 1.1 if reverse, expand y vector with the mirror data
    if (reverse) {
      z <- c(rev(y[2:(k+1)]),y,rev(y[(n-k):(n-1)]))
      n <- length(z)
    } else {
      z <- y
    }

    z0 <- switch(method,
                 hampel = qc_out_median(z, k, parent_logger),
                 tukey = qc_out_tukeyline(z, k, parent_logger),
                 quantile = qc_out_medianreg(z, k, parent_logger))
    z0.na <- !is.na(z0)
    z.na <- !is.na(z)
    ind <- NULL
    L <- 1.4826

    # STEP 2
    # Iteration loop
    for (i in (k + 1):(n - k)) {
      S0 <- L * median(abs(z[(i - k):(i + k)] - z0[i]), na.rm = TRUE)
      if (z0.na[i] & z.na[i] & !is.na(S0) & abs(z[i] - z0[i]) > t0 * S0) {
        z[i] <- z0[i]
        ind <- c(ind, i)
      }
    }

    # STEP 3
    # If reverse, drop the mirror data added before
    if (reverse) {
      z <- z[(k+1):(n-k)]
      ind <- ind-k
    }

    # STEP 4
    # Returning the res list
    return(list(res = z, index = ind))

    # END FUNCTION

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'out_hampel_filter',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'out_hampel_filter',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'out_hampel_filter',
                                                        sep = '.'))})
}

################################################################################
#' Outliers QC
#'
#' Outliers detection, substitution and annotation for SfnData objects
#'
#' Outliers for sap flow data and environmental data are located and substituted
#' by the selected method, using the \code{\link{out_hampel_filter}} function.
#'
#' @family Quality Checks Functions
#'
#' @param sfn_data SfnData object with the site data and metadata
#'
#' @param k window semi-value (integer) for substitution value calculation. See
#'   \code{\link{out_hampel_filter}} for details.
#'
#' @param t0 Integer value indicating the number of standard deviations for
#'   outlier detection threshold.
#'
#' @param method Character vector indicating the method to use in the outlier
#'   estimation: \code{hampel} (default), \code{tukey} and \code{quantile}.
#'
#' @param reverse Logical indicating if in case of k is bigger than the data
#'   length (i.e in the extremes of the vector) a reverse replication must be
#'   done. Default to TRUE.
#'
#' @param substitute Logical indicating if the outlier subsitution must be made.
#'   Default to FALSE.
#'
#' @return a SfnData object as the one provided with the oulier values
#'   substituted and the flags slot updated.
#'
#' @export

# START
# Function declaration
qc_out_remove <- function(sfn_data, k = 25L, t0 = 10L,
                       method = 'hampel', reverse = TRUE,
                       substitute = FALSE, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # SfnData
    if (!is(sfn_data, 'SfnData')) {
      stop('sfn_data object provided is not an SfnData object')
    }
    # k integer
    if (!is.integer(k)) {
      stop('k is not an integer')
    }
    # t0 integer
    if (!is.integer(t0)) {
      stop('t0 is not an integer')
    }
    # method character
    if (!is.character(method)) {
      stop('method is not a character')
    }

    # STEP 1
    # get needed data (without timestamp)
    sapf_data <- get_sapf(sfn_data)[,-1]
    env_data <- get_env(sfn_data)[,-1]
    sapf_flags <- get_sapf_flags(sfn_data)[,-1]
    env_flags <- get_env_flags(sfn_data)[,-1]

    # STEP 2
    # Apply selected outlier filter
    sapf_out <- lapply(sapf_data, qc_out_hampel_filter,
                       k = k, t0 = t0, method = method, reverse = reverse)
    env_out <- lapply(env_data, qc_out_hampel_filter,
                      k = k, t0 = t0, method = method, reverse = reverse)

    # STEP 3
    # Iteration for substituting each column of data with the filtered data

    # 3.1 sapf
    for (i in 1:ncol(sapf_data)) {
      # 3.1.1 substitute values if specified
      if (substitute) {
        sapf_data[, i] <- sapf_out[[i]][[1]]
        # 3.1.2 flags update
        old_flag <- sapf_flags[sapf_out[[i]][[2]], i]

        new_flag <- vapply(old_flag, function(x) {
          if (x == '') {
            "OUT_REPLACED"
          } else { paste0(x, "; OUT_REPLACED") }
        }, character(1), USE.NAMES = FALSE)

        sapf_flags[sapf_out[[i]][[2]], i] <- new_flag
      } else {
        # 3.1.2 flags update
        old_flag <- sapf_flags[sapf_out[[i]][[2]], i]

        new_flag <- vapply(old_flag, function(x) {
          if (x == '') {
            "OUT_WARN"
          } else { paste0(x, "; OUT_WARN") }
        }, character(1), USE.NAMES = FALSE)

        sapf_flags[sapf_out[[i]][[2]], i] <- new_flag
      }

    }

    # 3.2 env
    for (i in 1:ncol(env_data)) {
      # 3.2.1 substitute values if specified
      if (substitute) {
        env_data[, i] <- env_out[[i]][[1]]
        # 3.2.2 flags update
        old_flag <- env_flags[env_out[[i]][[2]], i]

        new_flag <- vapply(old_flag, function(x) {
          if (x == '') {
            "OUT_REPLACED"
          } else { paste0(x, "; OUT_REPLACED") }
        }, character(1), USE.NAMES = FALSE)

        env_flags[env_out[[i]][[2]], i] <- new_flag
      } else {
        # 3.2.2 flags update
        old_flag <- env_flags[env_out[[i]][[2]], i]

        new_flag <- vapply(old_flag, function(x) {
          if (x == '') {
            "OUT_WARN"
          } else { paste0(x, "; OUT_WARN") }
        }, character(1), USE.NAMES = FALSE)

        env_flags[env_out[[i]][[2]], i] <- new_flag
      }

    }

    # STEP 4
    # Update sfn_data and return it!!
    if (substitute) {
      get_sapf(sfn_data) <- sapf_data
      get_env(sfn_data) <- env_data
    }

    get_sapf_flags(sfn_data) <- sapf_flags
    get_env_flags(sfn_data) <- env_flags

    return(sfn_data)

    # END FUNCTION

  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'out_remove',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'out_remove',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'out_remove',
                                                        sep = '.'))})
}

################################################################################
#' Ranges dictionary
#'
#' Values for all ranges checked as a list
#'
#' To be abale to check for values out of ranges, first these ranges must be
#' described. This function generates a range dictionary for that mission. In
#' this way if ranges must be changed they only have to be changed in this
#' function.
#'
#' @family Quality Checks Functions
#'
#' @return A list containing the variable name as the element label and the
#'   range value as the element value.
#'
#' @export

# START
# Function declaration
qc_range_dic <- function(parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 1
    # Range values c(min,max)

    # env vars
    ta_range <- c(-30, 50) # no strict values, only as indicators,
    # slighty based on Cerveny et al. 2007
    rh_range <- c(0, 100) # common sense criteria
    vpd_range <- c(0, 40)
    sw_in_range <- c(0, 1362) # based in the solar constant
    ppfd_in_range <- c(0, 2400) # based in Rafa's comment
    netrad_range <- c(-280, 280) # based on Nasa maps
    ws_range <- c(0, 45) # based on Zhou & Wang, 2016 (Ameriflux)
    precip_range <- c(0, 250) # based on Cerveny et al. 2007
    swc_shallow_range <- c(0, 1) # cm3/cm3, max only can be 1
    swc_deep_range <- c(0, 1) # cm3/cm3, max only can be 1

    # sapf (we select maximum value as found in Manzoni 2013)
    # also we look for negative fluxes
    sapf_sapw_range <- c(0, 190) # units are cm3cm-2h-1
    sapf_tree_range <- c(0, 48000) # untis are cm3h-1

    # STEP 2
    # Create the res object
    res_ranges <- list(
      ta = ta_range,
      rh = rh_range,
      vpd = vpd_range,
      sw_in = sw_in_range,
      ppfd_in = ppfd_in_range,
      netrad = netrad_range,
      ws = ws_range,
      precip = precip_range,
      swc_shallow = swc_shallow_range,
      swc_deep = swc_deep_range,
      sapf_sapw = sapf_sapw_range,
      sapf_tree = sapf_tree_range
    )

    # STEP 3
    # Return the object
    return(res_ranges)
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_range_dic',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_range_dic',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_range_dic',
                                                        sep = '.'))})
}

################################################################################
#' Check ranges for sapflow
#'
#' Check ranges for sapflow
#'
#' Sapflow ranges are checked in sapwood or tree level, depending on the units
#' of the variable
#'
#' @family Quality Checks Functions
#'
#' @param sapf_data Sapflow data, as obtained from \code{\link{get_sapf}}
#'
#' @param plant_md Plant metadata, as obtained from \code{\link{get_plant_md}}
#'
#' @param sapf_flags Sapflow flags, as obtained from \code{\link{get_sapf_flags}}
#'
#' @return A data frame containing the new flags in case any value is out of
#'   range.
#'
#' @export

# START
# Function declaration
qc_sapf_ranges <- function(sapf_data, plant_md,
                           sapf_flags, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Check arguments
    if(!all(is.data.frame(sapf_data),
            is.data.frame(plant_md),
            is.data.frame(sapf_flags))) {
      stop('Data, metadata and/or flags objects provided are not data frames')
    }

    # STEP 1
    # 1.1 Get the dic
    ranges_dic <- qc_range_dic(parent_logger = parent_logger)

    # 1.2 Get the units
    sapf_units <- purrr::flatten_chr(plant_md[,'pl_sap_units'])
    names(sapf_units) <- purrr::flatten_chr(plant_md[,'pl_code'])

    # 1.3 Tranformation functions list
    funs_list <- list(
      '“cm3 cm-2 h-1”' = function(x) {x},
      '“cm3 m-2 s-1”' = function(x) {x/0.36},
      '“dm3 dm-2 h-1”' = function(x) {x/10},
      '“dm3 dm-2 s-1”' = function(x) {x/36000},
      '“mm3 mm-2 s-1”' = function(x) {x/360},
      '“g m-2 s-1”' = function(x) {x/0.36},
      '“kg m-2 h-1”' = function(x) {x/0.1},
      '“kg m-2 s-1”' = function(x) {x/360},
      '“cm3 s-1”' = function(x) {x/3600},
      '“cm3 h-1”' = function(x) {x},
      '“dm3 h-1”' = function(x) {x/1000},
      '“g h-1”' = function(x) {x},
      '“kg h-1”' = function(x) {x/1000}
    )

    # STEP 2
    # Get the out of range values

    # 2.1 start loop
    for (name in names(sapf_data[,-1])) {

      # 2.2 check if tree or sapwood level
      if (sapf_units[name] %in% names(funs_list)[1:8]) {
        # 2.2.1 sapwood
        range <- ranges_dic[['sapf_sapw']]
        range_transf <- vapply(
          range,
          funs_list[[sapf_units[[name]]]],
          numeric(1)
        )
      } else {
        # 2.2.2 tree
        range <- ranges_dic[['sapf_tree']]
        range_transf <- vapply(
          range,
          funs_list[[sapf_units[[name]]]],
          numeric(1)
        )
      }

      # 2.3 logical vector indicating if the value is out of range
      res_logical <- sapf_data[,name] < range_transf[1] | sapf_data[,name] > range_transf[2]

      # 2.4 vector with flags
      flags_vec <- sapf_flags[,name]

      # 2.5 inner loop
      for (i in 1:length(res_logical)) {
        if (!is.na(res_logical[i]) & res_logical[i]) {
          if (flags_vec[i] == '') {
            flags_vec[i] <- 'RANGE_WARN'
          } else {flags_vec[i] <- paste0(flags_vec[i], '; RANGE_WARN')}
        }
      }

      # 2.6 flags vector back to flags
      sapf_flags[,name] <- flags_vec
    }

    # STEP 3
    # Return the flag object
    return(sapf_flags)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_sapf_ranges',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_sapf_ranges',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_sapf_ranges',
                                                        sep = '.'))})
}

################################################################################
#' Check ranges for environmental variables
#'
#' Check ranges for environmental variables
#'
#' Environmental values are checked against known general ranges
#'
#' @family Quality Checks Functions
#'
#' @param env_data Environmental data, as obtained from \code{\link{get_env}}
#'
#' @param env_flags Environmental flags, as obtained from \code{\link{get_env_flags}}
#'
#' @return A data frame containing the new flags in case any value is out of
#'   range.
#'
#' @export

# START
# Function declaration
qc_env_ranges <- function(env_data, env_flags, parent_logger = 'test') {

  # Using calling handler to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if(!all(is.data.frame(env_data),
            is.data.frame(env_flags))) {
      stop('Data, and/or flags objects provided are not data frames')
    }

    # STEP 1
    # Ranges dic
    ranges_dic <- qc_range_dic(parent_logger = parent_logger)

    # STEP 2
    # Loop for each variable
    for (name in names(env_data[,-1])) {

      # 2.1 get the range for the variable
      range_var <- ranges_dic[[name]]

      # 2.2 logical vector indicating if the value is out of range
      res_logical <- env_data[, name] < range_var[1] | env_data[, name] > range_var[2]

      # 2.3 vector with flags
      flags_vec <- env_flags[,name]

      # 2.4 inner loop
      for (i in 1:length(res_logical)) {
        if (!is.na(res_logical[i]) & res_logical[i]) {
          if (flags_vec[i] == '') {
            flags_vec[i] <- 'RANGE_WARN'
          } else {flags_vec[i] <- paste0(flags_vec[i], '; RANGE_WARN')}
        }
      }

      # 2.5 flags vector back to flags
      env_flags[,name] <- flags_vec
    }

    # STEP 3
    # Return the env flags data frame
    return(env_flags)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_env_ranges',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_env_ranges',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_env_ranges',
                                                        sep = '.'))})
}

################################################################################
#' Checking for out of ranges values and flagging them
#'
#' This function checks for out of range values in sapflow and env data,
#' updating the flag slots in the SfnData object.
#'
#' @family Quality Checks Functions
#'
#' @param SfnData SfnData object to check for out of range values
#'
#' @return A SfnData object with the flags updated
#'
#' @export

# START
# Funtion declaration
qc_out_of_range <- function(SfnData, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (class(SfnData) != 'SfnData') {
      stop('Object provided is not a valid SfnData class object')
    }

    # STEP 1
    # Obtaining the needed data from the SfnData object
    sapf_data <- get_sapf(SfnData)
    env_data <- get_env(SfnData)
    sapf_flags <- get_sapf_flags(SfnData)
    env_flags <- get_env_flags(SfnData)
    plant_md <- get_plant_md(SfnData)

    # STEP 2
    # Get the new flags
    new_sapf_flags <- qc_sapf_ranges(sapf_data, plant_md, sapf_flags,
                                     parent_logger = parent_logger)

    new_env_flags <- qc_env_ranges(env_data, env_flags,
                                   parent_logger = parent_logger)

    # STEP 3
    # Update the SfnData object
    get_sapf_flags(SfnData) <- new_sapf_flags[,-1]
    get_env_flags(SfnData) <- new_env_flags[,-1]

    # STEP 4
    # Return the updated SfnData object
    return(SfnData)
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_out_of_range',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_out_of_range',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_out_of_range',
                                                        sep = '.'))})
}

################################################################################
#' Outliers process
#'
#' This function substitute the outliers based on the *_to_remove files found
#' in the out_warn folder for any site.
#'
#' @family Quality Checks Functions
#'
#' @param site character indicating the site to process
#'
#' @return an SfnData with the outliers and ranges substituted
#'
#' @export

qc_outliers_process <- function(site) {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.character(site)) {
      stop('site provided is not a character')
    }

    # STEP 1
    # Prepare the stuff
    # 1.1 Load the *_to_remove files
    out_to_remove <- readr::read_delim(
      file.path('Data', site, 'Lvl_2', 'lvl2_out_warn',
                paste0(site, '_out_to_remove.txt')),
      delim = ' '
    )

    ranges_to_remove <- readr::read_delim(
      file.path('Data', site, 'Lvl_2', 'lvl2_out_warn',
                paste0(site, '_ranges_to_remove.txt')),
      delim = ' '
    )

    # 1.2 load the SfnData
    sfn_data <- df_read_SfnData(site, 'out_warn', parent_logger = parent_logger)

    # 1.3 get the outliers substitution values (TIME CONSUMING STEP!!!!)
    sfn_data_out_rem <- qc_out_remove(sfn_data, substitute = TRUE,
                                      parent_logger = parent_logger)

    # STEP 2
    # Substitutes Time!!!
    # 2.1 get the sapf and env data and their flags
    sapf_data <- get_sapf(sfn_data)
    sapf_flags <- get_sapf_flags(sfn_data)
    sapf_data_out_rem <- get_sapf(sfn_data_out_rem)
    sapf_flags_out_rem <- get_sapf_flags(sfn_data_out_rem)
    env_data <- get_env(sfn_data)
    env_flags <- get_env_flags(sfn_data)
    env_data_out_rem <- get_env(sfn_data_out_rem)
    env_flags_out_rem <- get_env_flags(sfn_data_out_rem)

    # 2.2 substitute them!
    # 2.2.1 outliers
    for (i in 1:nrow(out_to_remove)) {

      index <- out_to_remove[['index']][i]
      variable <- out_to_remove[['variable']][i]

      if (variable %in% names(sapf_data)) {
        sapf_data[index, variable] <- sapf_data_out_rem[index, variable]

        # 2.2.1.1 update flags also
        sapf_flags[index, variable] <- sapf_flags_out_rem[index, variable]
      }

      if (variable %in% names(env_data)) {
        env_data[index, variable] <- env_data_out_rem[index, variable]

        # 2.2.1.1 update flags also
        env_flags[index, variable] <- env_flags_out_rem[index, variable]
      }
    }

    # 2.2.2 ranges (convert to NA because we can inpute them later)
    for (i in 1:nrow(ranges_to_remove)) {

      index <- ranges_to_remove[['index']][i]
      variable <- ranges_to_remove[['variable']][i]

      if (variable %in% names(sapf_data)) {
        sapf_data[index, variable] <- NA

        # 2.2.2.1 update flags also
        if (sapf_flags[index, variable] == '') {
          sapf_flags[index, variable] <- 'RANGE_REMOVE'
        } else {
          sapf_flags[index, variable] <- paste(sapf_flags[index, variable],
                                               '; RANGE_REMOVE')
        }
      }

      if (variable %in% names(env_data)) {
        env_data[index, variable] <- NA

        # 2.2.2.1 update flags also
        if (env_flags[index, variable] == '') {
          env_flags[index, variable] <- 'RANGE_REMOVE'
        } else {
          env_flags[index, variable] <- paste(env_flags[index, variable],
                                               '; RANGE_REMOVE')
        }
      }
    }

    # STEP 3
    # 3.1 Build the results
    get_sapf(sfn_data) <- sapf_data[,-1]
    get_sapf_flags(sfn_data) <- sapf_flags[,-1]
    get_env(sfn_data) <- env_data[,-1]
    get_env_flags(sfn_data) <- env_flags[,-1]

    # 3.2 return them!
    return(sfn_data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_outliers_process',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_outliers_process',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_outliers_process',
                                                        sep = '.'))})
}
