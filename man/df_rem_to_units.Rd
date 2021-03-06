% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_functions.R
\name{df_rem_to_units}
\alias{df_rem_to_units}
\title{Function to pass from out_rem to out_units}
\usage{
df_rem_to_units(parent_logger = "test")
}
\value{
Nothing, all the process is internal
}
\description{
REM to UNITS transfer
}
\details{
This function is in charge of check for sites ready to pass to out_units,
perform the available transformations for that site and write the resulting
SfnData in the destination folder (out_units)
}
\seealso{
Other Data Flow: \code{\link{df_accepted_to_lvl1}},
  \code{\link{df_copy_templates}},
  \code{\link{df_folder_structure}},
  \code{\link{df_get_data_folders}},
  \code{\link{df_get_status}},
  \code{\link{df_lvl1_to_lvl2}},
  \code{\link{df_lvl2_folder_structure}},
  \code{\link{df_read_SfnData}},
  \code{\link{df_received_to_accepted}},
  \code{\link{df_report_folder_creation}},
  \code{\link{df_reset_data_status}},
  \code{\link{df_set_status}},
  \code{\link{df_start_status}},
  \code{\link{df_warn_to_rem}},
  \code{\link{df_whos_ready_to}},
  \code{\link{df_write_SfnData}},
  \code{\link{sfn_data_constructor}}
}
