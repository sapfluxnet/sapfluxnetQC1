% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_unit_transformations.R
\name{qc_units_process}
\alias{qc_units_process}
\title{Units process}
\usage{
qc_units_process(sfndata, parent_logger = "test")
}
\arguments{
\item{sfndata}{SfnData object to perform the conversions}
}
\value{
An SfnData object with the newly calculated variables included
}
\description{
This function checks for available transformations and perform them if needed
}
\details{
List of available transformations are obtained from \code{\link{qc_transf_list}}
}
\seealso{
Other Unit conversion: \code{\link{qc_cm_cm_h}},
  \code{\link{qc_rh}}, \code{\link{qc_transf_list}},
  \code{\link{qc_transformation_vars}},
  \code{\link{qc_vpd}}
}
