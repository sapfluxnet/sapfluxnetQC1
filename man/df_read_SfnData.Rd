% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_functions.R
\name{df_read_SfnData}
\alias{df_read_SfnData}
\title{Load SfnData}
\usage{
df_read_SfnData(si_code, level = c("Lvl_1", "Lvl_2", "out_warn", "out_rem",
  "unit_trans"), units = NULL, parent_logger = "test")
}
\arguments{
\item{si_code}{Site code as a character string}

\item{level}{Level to read from as a character string}

\item{units}{Only used if \code{level = "unit_trans"}. Indicates which sapflow
units (plant, sapwood or leaf) must be read.}
}
\value{
A SfnData object.
}
\description{
Accesory function to load an specified SfnData object
}
\details{
Given a site code and a level description, \code{df_read_SfnData} will return
the selected SfnData object from the selected location
}
\seealso{
Other Data Flow: \code{\link{df_accepted_to_lvl1}},
  \code{\link{df_copy_templates}},
  \code{\link{df_folder_structure}},
  \code{\link{df_get_data_folders}},
  \code{\link{df_get_status}},
  \code{\link{df_lvl1_to_lvl2}},
  \code{\link{df_lvl2_folder_structure}},
  \code{\link{df_received_to_accepted}},
  \code{\link{df_rem_to_units}},
  \code{\link{df_report_folder_creation}},
  \code{\link{df_reset_data_status}},
  \code{\link{df_set_status}},
  \code{\link{df_start_status}},
  \code{\link{df_warn_to_rem}},
  \code{\link{df_whos_ready_to}},
  \code{\link{df_write_SfnData}},
  \code{\link{sfn_data_constructor}}
}
