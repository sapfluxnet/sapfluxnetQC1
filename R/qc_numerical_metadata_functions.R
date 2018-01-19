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
