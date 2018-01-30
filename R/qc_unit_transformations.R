################################################################################
#' Helper function for unit conversion
#'
#' \code{qc_get_sapw_md} allows to extract all necessary variables from plant
#' metadata in order to be able to convert sap flow units.
#'
#' @family Quality Checks Functions
#'
#' @param pl_metadata Data frame containing the plant metadata
#'
#' @return A data frame with extracted plant metadata variables as columns and
#'   individual plants as rows. Also a new variable \code{pl_sapw_area_est} is
#'   created as an empty numeric vector.
#'
#' @importFrom magrittr %>%
#'
#' @export

# START
# Function declaration
qc_get_sapw_md <- function(pl_metadata, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is pl_data a data frame?
    if (!is.data.frame(pl_metadata)) {
      stop('Provided pl_data object is not a data frame')
    }
    # Is the correct metadata? (Check for pl_code variable)
    if (is.null(pl_metadata$pl_code)) {
      stop('pl_code variable is missing from pl_data')
    }

    # STEP 1
    # Extract the desired variables
    res <- pl_metadata %>%
      dplyr::select(pl_code, pl_sap_units, pl_sapw_area, pl_leaf_area,
                    pl_dbh, pl_sapw_depth, pl_bark_thick) %>%
      dplyr::mutate(pl_sapw_area_est = 0)

    # STEP 2
    # Return the results
    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_get_sapw_md', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_get_sapw_md', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_get_sapw_md', sep = '.'))})

}

################################################################################
#' Sapwood area calculator
#'
#' Function to calculate sapwood area from DBH, bark thickness and sapwood depth
#'
#' In the case that sapwood area variable (\code{pl_sapw_area}) is not provided,
#' it still is needed to perform the unit conversion. In this case sapwood area
#' can be estimated from DBH and sapwood depth and bark thickness. If bark
#' thickness is not provided, estimation can be made but results should be
#' revised as, depending on the species, error in the estimates can be large.
#'
#' @section Ring area formula:
#' The area of a ring is \eqn{\pi(R² - r²)}, where \code{R} is the radius
#'   including the ring and \code{r} is the radius till the ring. Thus,
#'   \eqn{R = (dbh / 2) - bark_thickness} and
#'   \eqn{r = (dbh / 2) - bark_thickness - sapwood_depth}.
#'
#' @family Quality Checks Functions
#'
#' @param pl_vars Data frame containing the needed variables, usually the result
#'   of \code{\link{qc_get_sapw_md}}.
#'
#' @return A data frame, exactly as returned by \code{\link{qc_get_sapw_md}},
#'   but with the variable pl_sapw_area_est filled with the estimated values.
#'
#' @export

# START
# Function declaration
qc_sapw_area_calculator <- function(pl_vars, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Argument checking
    # is pl_vars a data frame?
    if (!is.data.frame(pl_vars)) {
      stop('Provided pl_vars object is not a data frame')
    }
    # has pl_vars the necessary variables?
    if (!all(c('pl_sapw_depth', 'pl_dbh',
               'pl_bark_thick', 'pl_sapw_area') %in% names(pl_vars))) {
      stop('Provided pl_vars object has not the needed variables to make ',
           'the conversion')
    }

    # STEP 1
    # Helper function to use in vapply
    helper <- function(i) {
      # mandatory variables + bark
      depth_dbh_bark <- all(!is.na(pl_vars$pl_sapw_depth[i]),
                            !is.na(pl_vars$pl_dbh[i]),
                            !is.na(pl_vars$pl_bark_thick[i]))
      # mandatory variables only
      depth_dbh <- all(!is.na(pl_vars$pl_sapw_depth[i]),
                       !is.na(pl_vars$pl_dbh[i]),
                       is.na(pl_vars$pl_bark_thick[i]))


      # if there is a sapwood area value, return it
      if (!is.na(pl_vars$pl_sapw_area[i])) {
        return(pl_vars$pl_sapw_area[i])
      } else {
        # if all mandatory variables and bark thickness are present, return the
        # estimate
        if (depth_dbh_bark) {
          return(pi*(((pl_vars$pl_dbh[i] / 2) - (pl_vars$pl_bark_thick[i]*0.1))^2 - ((pl_vars$pl_dbh[i] / 2) - (pl_vars$pl_bark_thick[i]*0.1) - pl_vars$pl_sapw_depth[i])^2))
        } else {
          # if all mandatory variables are present, but no bark thickness value
          # return estimate with a message
          if (depth_dbh) {
            message(pl_vars$pl_code[i], ' has no bark thickness value.',
                    ' Estimate of sapwood area must be taken with caution')
            return(pi*(((pl_vars$pl_dbh[i] / 2))^2 - ((pl_vars$pl_dbh[i] / 2) - pl_vars$pl_sapw_depth[i])^2))
          } else {
            # if one or more mandatory variables are missing, return NA with a
            # messege
            message(pl_vars$pl_code[i], ' has no sapwood depth and/or ',
                    'dbh values.', ' Estimate of sapwood area ',
                    'can not be calculated. Returning NA.')
            return(NA)
          }
        }
      }
    }

    # STEP 2
    # Calculate the estimates, if possible
    res_vec <- vapply(seq_along(pl_vars$pl_code), helper, numeric(1))

    # STEP 3
    # Return the results
    pl_vars$pl_sapw_area_est <- res_vec
    return(pl_vars)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_sapw_area_calculator', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_sapw_area_calculator', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_sapw_area_calculator', sep = '.'))})

}

################################################################################
#' Unit conversion
#'
#' Conversion of sap flow units to sapwood level
#' (cm³·cm⁻²·h⁻¹), plant level (cm³·h⁻¹) or leaf area level (cm³·cm⁻²·h⁻¹)
#'
#' @family Unit conversion
#'
#' @param x Numeric value in which the conversion must be done
#'
#' @param sapw_area Numeric value with the plant sapwood area in cm²
#'
#' @param leaf_area Numeric value with the plant leaf area in m²
#'
#' @param output_units Character vector indicating the kind of output units.
#'   Allowed values are \code{'plant'}, \code{'sapwood'} and \code{'leaf'}.
#'
#' @return A numeric value resulting from unit conversion
#'
#' @export

# START
# Function declaration
qc_cm_cm_h <- function(x, sapw_area, leaf_area, output_units,
                       parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*sapw_area
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*sapw_area)/(leaf_area*10000)
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_cm_cm_h', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_cm_cm_h', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_cm_cm_h', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from cm³·m⁻²·h⁻¹
#'
#' @export

# START
# Function declaration
qc_cm_m_s <- function(x, sapw_area, leaf_area, output_units,
                      parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x*0.36
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*sapw_area*0.36
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*sapw_area*1e-4*0.36)/(leaf_area)
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_cm_m_s', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_cm_m_s', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_cm_m_s', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from dm³·dm⁻²·h⁻¹
#'
#' @export

# START
# Function declaration
qc_dm_dm_h <- function(x, sapw_area, leaf_area, output_units,
                       parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x*10
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*sapw_area*10
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*sapw_area*1e-3)/(leaf_area)
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_dm_dm_h', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_dm_dm_h', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_dm_dm_h', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from dm³·dm⁻²·s⁻¹
#'
#' @export

# START
# Function declaration
qc_dm_dm_s <- function(x, sapw_area, leaf_area, output_units,
                       parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x*36000
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*sapw_area*36000
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*sapw_area*3.6)/(leaf_area)
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_dm_dm_s', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_dm_dm_s', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_dm_dm_s', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from mm³·mm⁻²·s⁻¹
#'
#' @export

# START
# Function declaration
qc_mm_mm_s <- function(x, sapw_area, leaf_area, output_units,
                       parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x*360
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*sapw_area*360
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*sapw_area*0.036)/(leaf_area)
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_mm_mm_s', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_mm_mm_s', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_mm_mm_s', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from g·m⁻²·s⁻¹
#'
#' @export

# START
# Function declaration
qc_g_m_s <- function(x, sapw_area, leaf_area, output_units,
                     parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x*0.36
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*sapw_area*0.36
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*sapw_area*0.36*1e-4)/(leaf_area)
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_g_m_s', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_g_m_s', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_g_m_s', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from kg·m⁻²·h⁻¹
#'
#' @export

# START
# Function declaration
qc_kg_m_h <- function(x, sapw_area, leaf_area, output_units,
                      parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x*1e-1
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*sapw_area*1e-1
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*sapw_area*1e-5)/(leaf_area)
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_kg_m_h', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_kg_m_h', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_kg_m_h', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from kg·m⁻²·s⁻¹
#'
#' @export

# START
# Function declaration
qc_kg_m_s <- function(x, sapw_area, leaf_area, output_units,
                      parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x*360
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*sapw_area*360
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*sapw_area*0.036)/(leaf_area)
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_kg_m_s', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_kg_m_s', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_kg_m_s', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from cm³·h⁻¹
#'
#' @export

# START
# Function declaration
qc_cm_h <- function(x, sapw_area, leaf_area, output_units,
                    parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x/sapw_area
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*1e-4)/leaf_area
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_cm_h', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_cm_h', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_cm_h', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from dm³·h⁻¹
#'
#' @export

# START
# Function declaration
qc_dm_h <- function(x, sapw_area, leaf_area, output_units,
                    parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- (x*1e3)/sapw_area
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*1e3
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*1e-1)/leaf_area
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_dm_h', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_dm_h', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_dm_h', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from cm³·s⁻¹
#'
#' @export

# START
# Function declaration
qc_cm_s <- function(x, sapw_area, leaf_area, output_units,
                    parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- (x*3600)/sapw_area
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*3600
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*0.36)/leaf_area
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_cm_s', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_cm_s', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_cm_s', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from g·h⁻¹
#'
#' @export

# START
# Function declaration
qc_g_h <- function(x, sapw_area, leaf_area, output_units,
                   parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- x/sapw_area
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*1e-4)/leaf_area
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_g_h', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_g_h', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_g_h', sep = '.'))})

}

################################################################################
#' @describeIn qc_cm_cm_h Conversion of sap flow units from kg·h⁻¹
#'
#' @export

# START
# Function declaration
qc_kg_h <- function(x, sapw_area, leaf_area, output_units,
                    parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # If x is NA, return NA to avoid check for numeric error
    if (is.na(x)) {return(NA)}
    # Are values numeric?
    if (any(!is.numeric(x), !is.numeric(sapw_area), !is.numeric(leaf_area))) {
      stop('x, sapw_area and/or leaf_area are not numeric values')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Sapwood
    if (output_units == 'sapwood') {
      if (all(is.na(sapw_area))) {
        stop('sapwood area values are missing')
      }
      res <- (x*1e3)/sapw_area
      return(res)
    } else {

      # STEP 2
      # Plant
      if (output_units == 'plant') {
        res <- x*1e3
        return(res)
      } else {

        # STEP 3
        # Leaf area
        if (output_units == 'leaf') {
          if (all(is.na(leaf_area))) {
            stop('leaf area values are missing')
          }
          res <- (x*1e-1)/leaf_area
          return(res)
        }
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_kg_h', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_kg_h', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_kg_h', sep = '.'))})

}

################################################################################
#' Sap flow units transformation
#'
#' Function to transform between sap flow units
#'
#' Sap flow accepted units can be of two kinds, \code{per sapwood area} and
#' \code{per plant}. Both kinds can come in many flavours and unit conversion
#' must be done to allow data integration and analysis.
#' This function can return three kind of units:
#' \describe{
#'   \item{\bold{Per sapwood  area}}{
#'   In this case, units returned are \eqn{cm³·cm⁻²·h⁻¹}
#'   }
#'   \item{\bold{Per plant}}{
#'   In this case, units returned are \eqn{cm³·h⁻¹}
#'   }
#'   \item{\bold{Per leaf area unit}}{
#'   In this case, units returned are \eqn{cm³·cm⁻²·h⁻¹}
#'   }
#' }
#'
#' @section Sapwood area:
#' If origin units are \emph{per sapwood area}, direct transformation is made. If
#' origin units are \emph{per plant} then \code{pl_sapw_area} variable from
#' plant metadata is needed to make the conversion.
#'
#' @section Plant:
#' If origin units are \emph{per plant}, direct transformation is made. If
#' origin units are \emph{per sapwood area} then \code{pl_sapw_area} variable
#' from plant metadata is needed to make the conversion.
#'
#' @section Leaf area:
#' If origin units are \emph{per plant} then \code{pl_leaf_area} variable from
#' plant metadata is needed to make the conversion. If origin units are
#' \emph{per sapwood area} then \code{pl_leaf_area} and \code{pl_sapw_area}
#' variables from plant metadata are needed.
#'
#' @section \code{pl_sapw_area}:
#' If \code{pl_sapw_area} is not available but \code{pl_sapw_depth} and
#' \code{pl_dbh} are provided, sapwood area value can be estimated by means of
#' \code{\link{qc_sapw_area_calculator}} function previous to the use of this
#' function.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the sap flow measurements
#'
#' @param sapw_md Data frame containing the sapwood metadata, as obtained from
#'   \code{\link{qc_get_sapw_md}} or \code{\link{qc_sapw_area_calculator}}.
#'
#' @param output_units Character vector indicating the kind of output units.
#'   Allowed values are \code{'plant'}, \code{'sapwood'} and \code{'leaf'}.
#'   See details to obtain more information
#'
#' @export

# START
# Function declaration
qc_sapw_conversion <- function(data, sapw_md, output_units = 'plant',
                               parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Arguments checking
    # Are data and sapw_md data frames?
    if (any(!is.data.frame(data), !is.data.frame(sapw_md))) {
      stop('data and/or sapw_md objects are not data frames')
    }
    # Is output units a character vector?
    if (!is.character(output_units)) {
      stop('output_units value is not a character vector')
    }
    # Is output units a valid value?
    if (!(output_units %in% c('plant', 'sapwood', 'leaf'))) {
      stop('output_units = "', output_units, '" is not a valid value. See function ',
           'help (?qc_sapw_conversion) for a list of valid values')
    }

    # STEP 1
    # Needed objects

    # 1.1 create a list/dictionary with the conversion functions
    funs_list <- list(
      '“cm3 cm-2 h-1”' = sapfluxnetQC1::qc_cm_cm_h,
      '“cm3 m-2 s-1”' = sapfluxnetQC1::qc_cm_m_s,
      '“dm3 dm-2 h-1”' = sapfluxnetQC1::qc_dm_dm_h,
      '“dm3 dm-2 s-1”' = sapfluxnetQC1::qc_dm_dm_s,
      '“mm3 mm-2 s-1”' = sapfluxnetQC1::qc_mm_mm_s,
      '“g m-2 s-1”' = sapfluxnetQC1::qc_g_m_s,
      '“kg m-2 h-1”' = sapfluxnetQC1::qc_kg_m_h,
      '“kg m-2 s-1”' = sapfluxnetQC1::qc_kg_m_s,
      '“cm3 s-1”' = sapfluxnetQC1::qc_cm_s,
      '“cm3 h-1”' = sapfluxnetQC1::qc_cm_h,
      '“dm3 h-1”' = sapfluxnetQC1::qc_dm_h,
      '“g h-1”' = sapfluxnetQC1::qc_g_h,
      '“kg h-1”' = sapfluxnetQC1::qc_kg_h
    )

    # 1.2 TIMESTAMP variable is not needed for the loop, drop it
    data_tmp <- data
    data_tmp$TIMESTAMP <- NULL

    # 1.3 Results data frame, here TIMESTAMP is needed
    res_df <- data.frame(TIMESTAMP = data$TIMESTAMP,
                         stringsAsFactors = FALSE)

    # STEP 2
    # Loop for each plant/tree
    for (code in names(data_tmp)) {

      # 3.1 units, sapw area and leaf area values
      sapw_units <- as.character(sapw_md[sapw_md[,'pl_code'] == code, 'pl_sap_units'])
      sapw_area <- as.numeric(sapw_md[sapw_md[,'pl_code'] == code, 'pl_sapw_area'])
      leaf_area <- as.numeric(sapw_md[sapw_md[,'pl_code'] == code, 'pl_leaf_area'])

      # 3.2 vapply to convert all the plant measures
      plant_res <- vapply(
        data_tmp[[code]],
        funs_list[[sapw_units]],
        numeric(1),
        sapw_area = sapw_area, leaf_area = leaf_area, output_units = output_units,
        parent_logger = parent_logger
      )

      # 3.3 add the plant results to the data frame results
      res_df[[code]] <- plant_res
    }

    # STEP 4
    # Return the results
    return(res_df)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_sapw_conversion', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_sapw_conversion', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_sapw_conversion', sep = '.'))})

}

################################################################################
#' Radiation units transformation
#'
#' Function to transform between radiation units
#'
#' Radiation accepted measures can be of two kinds, \code{incoming photosynthetic photon flux density}
#' and \code{shortwave incoming radiation}. Unit conversion must be done to allow data
#' integration and analysis.
#' This function converts between:
#' \describe{
#'   \item{\bold{Incoming photosynthetic photon flux density}}{
#'   In this case, units returned are \eqn{{\mu}mol·m⁻²·s⁻¹}
#'   }
#'   \item{\bold{Shortwave incoming radiation}}{
#'   In this case, units returned are \eqn{W·m⁻²}
#'   }
#' }
#'
#' @section Incoming photosynthetic photon flux density:
#' Direct transformation is made from \emph{shortwave incoming radiation}.
#'
#' @section Shortwave incoming radiation:
#' Direct transformation is made from \emph{incoming photosynthetic photon flux density}.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the environmental measurements
#'
#' @export

# START
# Function declaration
qc_rad_conversion <- function(data, parent_logger = 'test') {

  # Using calling handlers to logging
  withCallingHandlers({

    # STEP 0
    # Is data a data frame?
    if (!is.data.frame(data)) {
      stop('data object is not a data frame')
    }

    # STEP 1
    # Convert radiation values

    # 1.1 If both measures appear in the data frame, no transformation is made
    if (all(c('sw_in', 'ppfd_in') %in% names(data))){

      message('Radiation in both sw_in and ppfd_in units already exists. No transformation made.')

    # 1.2 If none of the measures appear in the data frame, no transformation is made,
    # with a warning
    } else if (all(!(c('sw_in', 'ppfd_in') %in% names(data)))){

      warning('Both sw_in and ppfd_in are missing. No transformation is possible.')

    # 1.3 If only sw_in appears in the data frame, transformation to ppfd_in is made
    } else if ('sw_in' %in% names(data)){

      # ppfd_in <- LakeMetabolizer::sw.to.par.base(data$sw_in)
      # data <- cbind(data, ppfd_in)
      # coefficient from Britton and Dodd (1976)
      data$ppfd_in <- data$sw_in * 2.114

    # 1.3 If only ppfd_in appears in the data frame, transformation to sw_in is made
    } else if ('ppfd_in' %in% names(data)){

      # sw_in <- LakeMetabolizer::par.to.sw.base(data$ppfd_in)
      # data <- cbind(data,sw_in)
      # coefficient from Britton and Dodd (1976)
      data$sw_in <- data$ppfd_in * 0.473

    }

    # STEP 2
    # Return the results
    return(data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger, 'qc_rad_conversion', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger, 'qc_rad_conversion', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger, 'qc_rad_conversion', sep = '.'))})

}

################################################################################
#' Soil texture classification
#'
#' Function to classify soil texture
#'
#' Using the percentage of clay, silt and sand in the soil, soil
#' texture is calculated using the USDA classification if the category
#' is not given in the data frame.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the data about the stand soil texture.
#'   It must have the variabes st_clay_perc, st_silt_perc, st_sand_perc and
#'   st_soil_texture.
#'
#'
#' @return The initial data frame with a new variable 'st_USDA_soil_texture'
#' which contains the soil texture if it is different from NA.
#'
#'
#' @export

# START
# Function declaration
qc_soil_texture <- function(data, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Is data a data.frame?
    if (!is.data.frame(data)) {
      stop('Data is not a data frame')
    }
    # Are there the st_clay_perc, st_silt_perc, st_sand_perc and
    # st_soil_texture variables in data?
    if (any(is.null(data$st_clay_perc), is.null(data$st_silt_perc),
            is.null(data$st_sand_perc), is.null(data$st_soil_texture))) {
      stop('At least one of the required variables is missing in data')
    }

    # STEP 1
    # names and temp objects
    data_perc <- data.frame(CLAY = data$st_clay_perc,
                            SILT = data$st_silt_perc,
                            SAND = data$st_sand_perc,
                            row.names = 'Percentage')

    list_abbr <- list(Cl = 'clay', SiCl = 'silty clay', SaCl = 'sandy clay',
                      ClLo = 'clay loam', SiClLo = 'silty clay loam',
                      SaClLo = 'sandy clay loam', Lo = 'loam', SiLo = 'silty loam',
                      SaLo = 'sandy loam', Si = 'silt', LoSa = 'loamy sand',
                      Sa = 'sand')

    # STEP 2

    # 2.1 : check missing data about %
    # IF there is any percentage missing for clay, silt or sand

    #We use IF to see if there is any NA in the data.frame :
    #     --> if there are some NA : we read the category in st_soil_texture
    #     --> if there aren't any NA : we use the function of the package
    #         'soil texture'.
    if (any(is.na(data_perc))) {

      # If there is no information about clay, silt and sand and neither for
      # soil texture, the data frame remains the same and a warning message
      # appears to say that there is no info about the soil texture.

      if (is.na(data$st_soil_texture)){
        warning('There is no information about the soil texture, ',
                'returning the original data')
        return(data)
      } else {
        data$st_USDA_soil_texture <- tolower(data$st_soil_texture)
        message('One or more percentages are missing. ',
                'Soil classification taken from the original data')
        return(data)
      }
    } else {

      # 2.2 check the format of data (% or decimal ?)
      # Are the percentage given in decimal (0.1 instead of 10%) ?
      # Conversion to percentage if it is the case.
      if (sum (data_perc) <= 1 ) {
        data_perc <- data_perc * 100
      }

      # Check sum of the %
      # Is the sum of the percentages of clay, silt and sand equal to 100 ?
      if (sum(data_perc) != 100) {
        warning('The sum of the different percentages of clay, silt and sand is not equal to 100% ',
                'and soil texture can not be calculated')
        return(data)
      } else {

        # STEP 3
        # Obtain the soil texture class
        # 3.1 Using the package 'soiltexture', find the class of the texture of the soil
        tmp_soil_texture <- soiltexture::TT.points.in.classes(
          tri.data = data_perc,
          class.sys = 'USDA.TT', # We use the USDA classification
          PiC.type = 't') %>%
          stringr::str_split(., ", ")
        data$st_USDA_soil_texture <- list_abbr[[tmp_soil_texture[[1]][[1]]]]

        # 3.2 If soil texture class was already provided, check that it matches
        if (!is.na(data$st_soil_texture)){
          if (data$st_USDA_soil_texture != tolower(as.character(data$st_soil_texture))){
            warning('Calculated soil texture class differs ',
                    'from the class in the original data.')
          }
        }

        # 3.3 Return the data
        return(data)
      }
    }

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_soil_texture', sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_soil_texture', sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_soil_texture', sep = '.'))})
}

################################################################################
#' Solar time conversion
#'
#' Calculate the Extraterrestrial Radiation from the TIMESTAMP
#'
#' This function uses several functions from \code{solaR} package in order to
#' obtain the mean solar time, the equation of time for each day included in the
#' TIMESTAMP and the extraterrestrial radiation for each step of the TIMESTAMP.
#'
#' @section Apparent (Real) Solar Time:
#' The Apparent Solar Time is calculated as:
#' \deqn{Apparent Solar Time = Mean Solar Time + Equation of Time}
#' The Equation of Time is calculated for each day, whereas the Mean Solar Time
#' is calculated for each step of the TIMESTAMP.
#'
#' @family Quality Checks Functions
#'
#' @param data Environmental data frame containing the TIMESTAMP variable.
#'
#' @param site_md Data frame containing the latitude and longitude variables of
#'   the site (\code{si_lat} and \code{si_long})
#'
#' @param add_solar_ts Logical indicating if solar timestamp must be added to
#'   the environmental data frame.
#'
#' @return A data frame exactly as \code{data}, but with an additional column
#'   containing the extraterrestrial radiation in W/m2, and optionally another
#'   column containing apparent solar timestamp.
#'
#' @export

# START
# Function declaration
qc_ext_radiation <- function(data, site_md, add_solar_ts = FALSE,
                             parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    # Are data and site_md data frames?
    if(any(!is.data.frame(data), !is.data.frame(site_md))) {
      stop('data and/or site_md are not data frames')
    }
    # have data the timestamp variable?
    if(is.null(data$TIMESTAMP)) {
      stop('data has not a TIMESTAMP variable')
    }
    # have metadata objects the mandatory variables?
    if(any(is.null(site_md$si_lat), is.null(site_md$si_long))) {
      stop('site_md have not the needed variables. ',
           'See function help (?qc_solar_timestamp)')
    }
    # Is type a valid value?
    if (!is.logical(add_solar_ts)) {
      stop('add_solar_ts must be either TRUE or FALSE')
    }

    # STEP 1
    # Retrieve the accessory info
    lat <- site_md$si_lat
    long <- site_md$si_long
    timestamp <- data$TIMESTAMP

    # STEP 2
    # Intermediate objects
    # 2.2 Mean Solar Time
    mst <- solaR::local2Solar(timestamp, long)

    # STEP 3
    # Calculating Apparent Solar Time (Mean Solar Time + Equation of Time)
    # 2.1 Equation of time
    solD <- solaR::fSolD(lat, mst)
    EoT <- solaR::r2sec(solD$EoT)

    ast <- lapply(as.Date(strptime(zoo::index(EoT), format = '%Y-%m-%d')),
                  function(id, vect)
                    (vect[as.Date(vect) == id] +
                       zoo::coredata(EoT)[which(as.Date(strptime(zoo::index(EoT),
                                                                 format = '%Y-%m-%d')) == id)]),
                  vect = mst)

    ast <- do.call("c", ast)

    solD <- solaR::fSolD(lat, ast)
    solI <- zoo::coredata(solaR::fSolI(solD, BTi = ast)$Bo0)

    data$ext_rad <- solI

    if (add_solar_ts){
      data$solarTIMESTAMP <- ast
    }

    # STEP 4
    # Return data frame with new columns
    return(data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_ext_radiation',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_ext_radiation',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_ext_radiation',
                                                        sep = '.'))})
}

################################################################################
#' Calculation of VPD from rh and ta
#'
#' Calculate the VPD if needed from other environmental variables
#'
#' The calculations for this function were obtained from the REddyProc R package
#' (https://cran.r-project.org/package=REddyProc) by the MPI-BGC in Jena
#' licensed under GPL > 2
#'
#' @family Unit conversion
#'
#' @param data Data frame containing the environmental data
#'
#' @return a Data frame of the environmental data with the new VPD variable
#'   calculated
#'
#' @export

# START
# Function declaration
qc_vpd <- function(data, parent_logger = 'test') {

  # using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Check arguments
    # data frame
    if (!is.data.frame(data)) {
      stop("data object is not a data frame")
    }

    # check variables
    if (any(is.null(data[['rh']]), is.null(data[['ta']]))) {
      stop("data not contains rh and/or ta variables")
    }

    # check if vpd already exists
    if (!is.null(data[['vpd']])) {
      stop("data already has a vpd variable. Please revise if there",
           " is really necessary to recalculate vpd")
    }

    # STEP 1
    # Get the values of ta and rh
    ta <- data[['ta']]
    rh <- data[['rh']]

    # STEP 2
    # Calculate the VPD
    vpd <- 6.1078 * (1 - rh / 100) * exp(17.08085 * ta / (234.175 + ta))

    # STEP 3
    # Build the data res object
    data[['vpd']] <- vpd

    # STEP 4
    # Return the data
    return(data)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_vpd',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_vpd',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_vpd',
                                                        sep = '.'))})
}

################################################################################
#' Presence of variables needed for transformations summary
#'
#' Summary table for variables needed for unit and any other kind of conversions
#'
#' This function generates a table (data frame) with information about the
#' presence of the variables needed for unit, radiation, solar time and others
#' data transformations/conversions.
#'
#' @family Unit conversion
#'
#' @param sfndata SfnData object for the site containing all the information
#'
#' @return A data frame with the following columns:
#'
#'   \itemize{
#'     \item{Variable: Variable name}
#'     \item{Location: Variable location (i.e. env_data or env_md)}
#'     \item{Trasformation: Tranformation/Conversion for whihc the variable is needed}
#'     \item{Presence: Logical indicating if the variable is present}
#'   }
#'
#' @export

# START
# Function declaration
qc_transformation_vars <- function(sfndata, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Checking arguments
    # Is sfndata a SfnData object?
    if (!is(sfndata, "SfnData")) {
      stop('Data provided is not a SfnData object')
    }

    # STEP 1
    # Radiation conversion
    rad_vars <- c('sw_in', 'ppfd_in')
    rad_loc <- rep('env_data', length(rad_vars))
    rad_transf <- rep('radiation_conversion', length(rad_vars))
    rad_presence <- c(
      !is.null(get_env(sfndata)$sw_in),
      !is.null(get_env(sfndata)$ppfd_in)
    )

    # STEP 2
    # Extraterrestrial radiation
    exr_vars <- c('TIMESTAMP', 'si_lat', 'si_long')
    exr_loc <- c('env_data', 'site_md', 'site_md')
    exr_transf <- rep('solar_time', length(exr_vars))
    exr_presence <- c(
      !all(is.na(get_env(sfndata)$TIMESTAMP)),
      !all(is.na(get_site_md(sfndata)$si_lat)),
      !all(is.na(get_site_md(sfndata)$si_long))
    )

    # STEP 3
    # Sapflow unit transformations
    sfu_vars <- c('pl_sap_units', 'pl_sapw_area', 'pl_leaf_area')
    sfu_loc <- rep('plant_md', length(sfu_vars))
    sfu_transf <- rep('sapf_units', length(sfu_vars))
    sfu_presence <- c(
      !all(is.na(get_plant_md(sfndata)$pl_sap_units)),
      !all(is.na(get_plant_md(sfndata)$pl_sapw_area)),
      !all(is.na(get_plant_md(sfndata)$pl_leaf_area))
    )

    # STEP 4
    # VPD calculation
    vpd_vars <- c('rh', 'ta', 'vpd')
    vpd_loc <- rep('env_data', length(vpd_vars))
    vpd_transf <- rep('vpd_calc', length(vpd_vars))
    vpd_presence <- c(
      !is.null(get_env(sfndata)$rh),
      !is.null(get_env(sfndata)$ta),
      !is.null(get_env(sfndata)$vpd)
    )

    # STEP n
    # combining vector for each transformation in a data frame
    vars <- c(rad_vars, exr_vars, vpd_vars, sfu_vars)
    loc <- c(rad_loc, exr_loc, vpd_loc, sfu_loc)
    transf <- c(rad_transf, exr_transf, vpd_transf, sfu_transf)
    presence <- c(rad_presence, exr_presence, vpd_presence, sfu_presence)

    # n.1 data frame
    res <- data.frame(
      Variable = vars,
      Location = loc,
      Transformation = transf,
      Presence = presence,
      stringsAsFactors = FALSE
    )

    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_transformation_vars',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_transformation_vars',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_transformation_vars',
                                                        sep = '.'))})
}

################################################################################
#' Transformations list
#'
#' Show all the transformations indicating which ones can be done with the data
#' provided.
#'
#' The data frame returned by this function is intended to use it in the next
#' level, allowing automatized transformations depending on the available
#' variables.
#'
#' @family Unit conversion
#'
#' @param transf_info Data frame with info about the variables needed for
#'   transformations as generated by \code{\link{qc_transformation_vars}}
#'
#' @return A data frame with the following columns:
#'
#'   \itemize{
#'     \item{Transformation: Transformation/Conversion name}
#'     \item{Available: Logical indicating if the transformation is possible}
#'   }
#'
#' @export

# START
# Function declaration
qc_transf_list <- function(transf_info, parent_logger = 'test') {

  # Using calling handlers to manage errors
  withCallingHandlers({

    # STEP 0
    # Argument checks
    if (!is.data.frame(transf_info)) {
      stop('trans_info provided is not a data.frame')
    }

    # STEP 1
    # Radiation conversion
    rad_transf <- 'radiation_conversion'
    rad_info <- transf_info %>%
      dplyr::filter(Transformation == 'radiation_conversion')
    if (all(rad_info$Presence) | all(!rad_info$Presence)) {
      rad_avail <- FALSE
    } else {
      rad_avail <- TRUE
    }

    # STEP 2
    # Extraterrestrial radiation
    exr_trasnf <- 'solar_time'
    exr_info <- transf_info %>%
      dplyr::filter(Transformation == 'solar_time')
    if (all(exr_info$Presence)) {
      exr_avail <- TRUE
    } else {exr_avail <- FALSE}

    # STEP 3
    # Sapflow unit conversions
    sfu_info <- transf_info %>%
      dplyr::filter(Transformation == 'sapf_units')

    # 3.1 plant level
    sfu_info_plant <- sfu_info %>%
      dplyr::filter(Variable != 'pl_leaf_area')
    sfu_plant_trasnf <- 'sapf_units_to_plant'
    sfu_plant_avail <- all(sfu_info_plant$Presence)

    # 3.2 sapwood level (is the same that for plant)
    sfu_sapw_trasnf <- 'sapf_units_to_sapwood'
    sfu_sapw_avail <- all(sfu_info_plant$Presence)

    # 3.3 leaf area level
    sfu_info_leaf <- sfu_info
    sfu_leaf_transf <- 'sapf_units_to_leaf_area'
    sfu_leaf_avail <- all(sfu_info_leaf$Presence)

    # STEP 4
    # VPD calculation
    vpd_info <- transf_info %>%
      dplyr::filter(Transformation == 'vpd_calc')
    vpd_transf <- 'VPD_calculation'

    if (vpd_info[3, 'Presence']) {
      vpd_avail <- FALSE
    } else {
      if (!vpd_info[1, 'Presence'] | !vpd_info[2, 'Presence']) {
        vpd_avail <- FALSE
      } else {
        vpd_avail <- TRUE
      }
    }

    # STEP n
    # build res data frame and return it
    transf <- c(rad_transf, exr_trasnf, vpd_transf, sfu_plant_trasnf,
                sfu_sapw_trasnf, sfu_leaf_transf)
    avail <- c(rad_avail, exr_avail, vpd_avail, sfu_plant_avail,
               sfu_sapw_avail, sfu_leaf_avail)

    res <- data.frame(
      Transformation = transf,
      Available = avail,
      stringsAsFactors = FALSE
    )

    return(res)

    # END FUNCTION
  },

  # handlers
  warning = function(w){logging::logwarn(w$message,
                                         logger = paste(parent_logger,
                                                        'qc_transf_list',
                                                        sep = '.'))},
  error = function(e){logging::logerror(e$message,
                                        logger = paste(parent_logger,
                                                       'qc_transf_list',
                                                       sep = '.'))},
  message = function(m){logging::loginfo(m$message,
                                         logger = paste(parent_logger,
                                                        'qc_transf_list',
                                                        sep = '.'))})
}
