# ################################################################################
# #' Find if provided values of a numerical variable are in a suitable range
# #'
# #' \code{qc_suitable_range} is used to determine if a variable has strange
# #' values out of natural or biological expected range.
# #'
# #' Checks are made based on the variable and acceptable ranges provided.
# #' \code{variables} and \code{ranges} must be of the same length, as the first
# #' variable is compared with the first range provided, and so on. If both
# #' objects are not of the same length, an informative error appears and function
# #' stops.
# #'
# #' @family Quality Check Functions
# #'
# #' @param data Data frame containing the variables to check
# #'
# #' @param variables Character vector with variables names
# #'
# #' @param ranges List in the form of
# #'   \code{list("variable1" = c(min, max), "variable2" = c(min, max)...)}
# #'   indicating the suitable ranges for every variable contained in
# #'   \code{variable} (See details).
# #'
# #' @return A logical vector indicating if variables are in range
# #'
# #' @export
#
#
# # START
# # Function declaration
#
# qc_suitable_range <- function(data, variables, ranges,
#                               parent_logger = 'test') {
#
#   # Using calling handlers to logging
#   withCallingHandlers({
#
#     # STEP 0
#     # Argument checks
#     # Is ranges a list?
#     if (!is.list(ranges)) {
#       stop('ranges argument provided is not a list, please provide a list in
#            the form of list("variable1" = c(min, max), "variable2" = c(min, max)...)')
#     }
#     # Is data a data.frame?
#     if (!is.data.frame(data)) {
#       stop('provided data is not a data.frame, please review data object provided')
#     }
#     # Is variables a character vector?
#     if (!is.character(variables)) {
#       stop('variables are not provided as character vector, please review
#            variables object')
#     }
#     # Are variables and ranges of the same length?
#     if (length(variables) != length(ranges)) {
#       stop('variables vector and ranges list are not of the same length, please
#            review provided objects')
#     }
#     # Are variables present in data?
#     if (!all(variables %in% data)) {
#       stop('one or more variables are nor present in data, please make sure that
#            provided data and variables are correct')
#   }
#
#   # STEP 1
#   # Extract the variable values
#   var_values <- lapply(variables, function(x) {
#     data[[x]]
#   })
#
#   # STEP 2
#   # Check if values are in range
#
#   # 2.1 Empty results vector
#   res <- vector()
#   # 2.2 For loop to see if each variable value is in range
#   for (i in 1:length(var_values)) {
#     tmp_res <- var_values[i] >= ranges[[i]][1] && var_values[i] <= ranges[[i]][2]
#     res <- c(res, tmp_res)
#   }
#
#   # STEP 3
#   # Return the results
#   return(res)
#   # END FUNCTION
#   },
#
#   # handlers
#   warning = function(w){logging::logwarn(w$message,
#                                          logger = paste(parent_logger,
#                                                         'qc_suitable_range', sep = '.'))},
#   error = function(e){logging::logerror(e$message,
#                                         logger = paste(parent_logger,
#                                                        'qc_suitable_range', sep = '.'))},
#   message = function(m){logging::loginfo(m$message,
#                                          logger = paste(parent_logger,
#                                                         'qc_suitable_range', sep = '.'))})
#
# }
#
# ################################################################################
# #' Suitable Range for sapflow values
# #'
# #' Suitability check for sapflow values.
# #'
# #' Ranges for sapflow measures at sapwood level are obtained from literature,
# #' being the most extreme values -10 and 533 cm3cm-2h-1 (Manzoni et al 2013 and
# #' other sources). This range is only informative, but values outside of the
# #' range may indicate some error in the data.
# #'
# #' @family Quality Checks Functions
# #'
# #' @param data Data frame with the values of sapflow measures for each plant
# #'   (variable).
# #'
# #' @return A data frame with summarising the values outside of the suitable
# #'   range.
# #'
# #' @export
#
# # START
# # Function declaration
# qc_sapf_range_check <- function(data, parent_logger = 'test') {
#
#   # Using calling handlers to manage errors
#   withCallingHandlers({
#
#     # STEP 0
#     # Argument checks
#     # is data a data frame?
#     if(!is.data.frame(data)) {
#       stop('data provided is not a data frame')
#     }
#
#     # STEP 1
#     # Initialising res objects
#     names <- names(data[,-1])
#     plant <- vector()
#     value <- vector()
#
#     # STEP 2
#     # For loop to iterate between plants
#     for (name in names) {
#       value <- c(value, data[data[, name] < -10 | data[, name] > 533, name])
#       plant <- c(plant, rep(name,
#                             length(data[data[, name] < -10 | data[, name] > 533, name])))
#     }
#
#     # STEP 3
#     # Build the results data frame
#     res <- data.frame(
#       Plant = plant,
#       Value = value,
#       Max = 533,
#       Min = -10,
#       stringsAsFactors = FALSE
#     )
#
#     # 3.1 Return the results
#     return(res)
#
#   },
#
#   # handlers
#   warning = function(w){logging::logwarn(w$message,
#                                          logger = paste(parent_logger,
#                                                         'qc_sapf_range_check', sep = '.'))},
#   error = function(e){logging::logerror(e$message,
#                                         logger = paste(parent_logger,
#                                                        'qc_sapf_range_check', sep = '.'))},
#   message = function(m){logging::loginfo(m$message,
#                                          logger = paste(parent_logger,
#                                                         'qc_sapf_range_check', sep = '.'))})
# }

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
    ppdf_in_range <- c(0, 2400) # based in Rafa's comment
    netrad_range <- c(-280, 280) # based on Nasa maps
    ws_range <- c(0, 45) # based on Zhou & Wang, 2016 (Ameriflux)
    precip_range <- c(0, 250) # based on Cerveny et al. 2007

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
      ppdf_in = ppdf_in_range,
      netrad = netrad_range,
      ws = ws_range,
      precip = precip_range,
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
    ranges_dic <- qc_ranges_dic(parent_logger = parent_logger)

    # 1.2 Get the units
    sapf_units <- plant_md[,'pl_sap_units']
    # TODO
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
