% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_md_general_checks.R
\name{qc_env_vars_presence}
\alias{qc_env_vars_presence}
\title{Check for environmental variables presence}
\usage{
qc_env_vars_presence(data, metadata, parent_logger = "test")
}
\arguments{
\item{data}{Data frame containing the environmental data}

\item{metadata}{Data frame containing the environmental metadata}
}
\value{
A data frame with four columns: Variable name, Presence in metadata,
  Presence in data and Concordance.
}
\description{
\code{qc_env_vars_presence} function checks if environmental variables stated
in metadata (environmental_md sheet) are really present in the environmental
data.
}
\details{
The function get all the names of the possible environmental variables and
check the presence in metadata and data. If there is any inconsistency
between them.
}
\seealso{
Other Quality Checks Functions: \code{\link{create_dic}},
  \code{\link{qc_coordinates}},
  \code{\link{qc_data_results_table}},
  \code{\link{qc_env_ranges}},
  \code{\link{qc_ext_radiation}},
  \code{\link{qc_factor_values}},
  \code{\link{qc_fix_timestamp}},
  \code{\link{qc_get_biomes_spdf}},
  \code{\link{qc_get_biome}}, \code{\link{qc_get_sapw_md}},
  \code{\link{qc_get_timestep}},
  \code{\link{qc_get_timezone}},
  \code{\link{qc_is_timestamp}}, \code{\link{qc_md_cols}},
  \code{\link{qc_md_results_table}},
  \code{\link{qc_mind_the_gap_eff}},
  \code{\link{qc_mind_the_gap}},
  \code{\link{qc_out_hampel_filter}},
  \code{\link{qc_out_of_range}},
  \code{\link{qc_out_remove}},
  \code{\link{qc_outliers_process}},
  \code{\link{qc_outliers_subs}},
  \code{\link{qc_pl_treatments}},
  \code{\link{qc_rad_conversion}},
  \code{\link{qc_range_dic}}, \code{\link{qc_sapf_ranges}},
  \code{\link{qc_sapw_area_calculator}},
  \code{\link{qc_sapw_conversion}},
  \code{\link{qc_set_timezone}},
  \code{\link{qc_soil_texture}},
  \code{\link{qc_species_names_info}},
  \code{\link{qc_species_names}},
  \code{\link{qc_species_verification}},
  \code{\link{qc_start_process}},
  \code{\link{qc_swc_check}}, \code{\link{qc_swc_fix}},
  \code{\link{qc_time_interval}},
  \code{\link{qc_timestamp_concordance}},
  \code{\link{qc_timestamp_errors}},
  \code{\link{qc_timestamp_nas}}
}
