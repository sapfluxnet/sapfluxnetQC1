% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dl_data_loading_functions.R
\name{dl_metadata}
\alias{dl_metadata}
\title{Loading metadata from xls/xlsx}
\usage{
dl_metadata(file_name, sheet_name, si_code_loc = NULL,
  parent_logger = "test")
}
\arguments{
\item{file_name}{Character vector indicating the name of the xls/xlsx file
containing the metadata.}

\item{sheet_name}{Character vector indicating the name of the sheet to be
loaded. It must be one of \code{site_md}, \code{stand_md}, \code{species_md},
\code{plant_md} or \code{environmental_md}.}

\item{si_code_loc}{Name of the object containing the site metadata, in order
to obtain si_code variable to include it in other metadata objects. Default
to \code{NULL}, as the first metadata to load must be the site metadata.}
}
\value{
The function returns a data_frame with the corresponding metadata
  (site, stand, species, plant or environmental) in "wide" format, with
  metadata variables as columns, ready to be feeded to quality check
  functions.
}
\description{
\code{dl_metadata} function loads the metadata sheets from xls/xlsx file.
}
\details{
This function make use of dplyr, tidyr and readxl packages in order to
retrieve and format the metadata. It's intended to be used as first step
to load the metadata and start the metadata quality check.
}
\seealso{
Other Data Loading Functions: \code{\link{dl_data_col_classes}},
  \code{\link{dl_data}}, \code{\link{dl_dec_char_detect}},
  \code{\link{dl_get_si_code}},
  \code{\link{dl_na_char_generator}},
  \code{\link{qc_as_timestamp}},
  \code{\link{remove_dupcols}}
}
