################################################################################
#' Helper function for unit conversion
#'
#' \code{qc_pl_units_helper} allows to extract all necessary variables from plant
#' metadata in order to be able to convert sap flow units.
#'
#' @family Quality Checks Functions
#'
#' @param pl_data Data frame containing the plant metadata
#'
#' @return A data frame with extracted plant metadata variables as columns and
#'   individual plants as rows.
#'
#' @importFrom magrittr %>%
#'
#' @export

# START
# Function declaration
qc_pl_units_helper <- function(pl_data) {

  # STEP 0
  # Argument checks
  # Is pl_data a data frame?
  if (!is.data.frame(pl_data)) {
    stop('Provided pl_data object is not a data frame')
  }
  # Is the correct metadata? (Check for pl_code variable)
  if (is.null(pl_data$pl_code)) {
    stop('pl_code variable is missing from pl_data')
  }

  # STEP 1
  # Extract the desired variables
  res <- pl_data %>%
    dplyr::select(pl_code, pl_sapw_units, pl_sapw_area, pl_leaf_area)

  # STEP 2
  # Return the results
  return(res)

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

# START
# Function declaration
qc_sapw_conversion <- function() {

  # STEP 0
  # Arguments checking
}
