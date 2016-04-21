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
#'   \item{Per sapwood  area}{In this case, units returned are}
#'   \item{Per plant}{In this case, units returned are}
#'   \item{Per leaf area unit}{In this case, units returned are}
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
#' @section \code{pl_sapw_area}
#' If \code{pl_sapw_area} is not available but \code{pl_sapw_depth} and
#' \code{pl_dbh} are provided, sapwood area value can be estimated and used
#' in the unit conversion.
#'
