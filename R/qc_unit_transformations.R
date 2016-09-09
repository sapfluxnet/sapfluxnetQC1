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
#'   Allowed values are \code{"plant"}, \code{"sapwood"} and \code{"leaf"}.
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
#'   Allowed values are \code{"plant"}, \code{"sapwood"} and \code{"leaf"}.
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
    # Are data and pl_metadata data frames?
    if (any(!is.data.frame(data), !is.data.frame(sapw_md))) {
      stop('data and/or pl_metadata objects are not data frames')
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
      '“cm3 cm-2 h-1”' = sapfluxnetr::qc_cm_cm_h,
      '“cm3 m-2 s-1”' = sapfluxnetr::qc_cm_m_s,
      '“dm3 dm-2 h-1”' = sapfluxnetr::qc_dm_dm_h,
      '“dm3 dm-2 s-1”' = sapfluxnetr::qc_dm_dm_s,
      '“mm3 mm-2 s-1”' = sapfluxnetr::qc_mm_mm_s,
      '“g m-2 s-1”' = sapfluxnetr::qc_g_m_s,
      '“kg m-2 h-1”' = sapfluxnetr::qc_kg_m_h,
      '“kg m-2 s-1”' = sapfluxnetr::qc_kg_m_s,
      '“cm3 s-1”' = sapfluxnetr::qc_cm_s,
      '“cm3 h-1”' = sapfluxnetr::qc_cm_h,
      '“dm3 h-1”' = sapfluxnetr::qc_dm_h,
      '“g h-1”' = sapfluxnetr::qc_g_h,
      '“kg h-1”' = sapfluxnetr::qc_kg_h
    )

    # 1.2 TIMESTAMP variable is not needed for the loop, drop it
    data_tmp <- data
    data_tmp$TIMESTAMP <- NULL

    # 1.3 Results data frame, here TIMESTAMP is needed
    res_df <- data.frame(TIMESTAMP = data$TIMESTAMP)

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
