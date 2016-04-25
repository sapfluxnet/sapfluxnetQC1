################################################################################
#' Helper function for unit conversion
#'
#' \code{qc_get_pl_md} allows to extract all necessary variables from plant
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
qc_get_pl_md <- function(pl_metadata) {

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
#'   of \code{\link{qc_get_pl_md}}.
#'
#' @return A data frame, exactly as returned by \code{\link{qc_get_pl_md}},
#'   but with the variable pl_sapw_area_est filled with the estimated values.
#'
#' @export

# START
# Function declaration
qc_sapw_area_calculator <- function(pl_vars) {

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
#'   \item{Per sapwood  area}{
#'   In this case, units returned are \eqn{m³/m²·s}
#'   }
#'   \item{Per plant}{
#'   In this case, units returned are \eqn{m³/s}
#'   }
#'   \item{Per leaf area unit}{
#'   In this case, units returned are \eqn{m³/m²·s}
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
#' \code{pl_dbh} are provided, sapwood area value can be estimated and used
#' in the unit conversion.
#'
#' @family Quality Checks Functions
#'
#' @param data Data frame containing the sap flow measurements
#'
#' @param pl_metadata Data frame containing the plant metadata
#'
#' @param output_units Character vector indicating the kind of output units.
#'   Allowed values are \code{"plant"}, \code{"sapwood"} and \code{"leaf"}.
#'   See details to obtain more information
#'
#' @export

# START
# Function declaration
qc_sapw_conversion <- function(data, pl_metadata, output_units = 'plant') {

  # STEP 0
  # Arguments checking
  # Are data and pl_metadata data frames?
  if (any(!is.data.frame(data), !is.data.frame(pl_metadata))) {
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
  # per plant units output
  if (output_units == 'plant') {

    # 1.1 obtain the metadata variables to know the input units
    metadata_vars <- qc_get_pl_md(pl_metadata)

    # 1.2

  }
}
