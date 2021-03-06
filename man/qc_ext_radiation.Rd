% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_unit_transformations.R
\name{qc_ext_radiation}
\alias{qc_ext_radiation}
\title{Solar time conversion}
\usage{
qc_ext_radiation(data, site_md, add_solar_ts = FALSE,
  parent_logger = "test")
}
\arguments{
\item{data}{Environmental data frame containing the TIMESTAMP variable.}

\item{site_md}{Data frame containing the latitude and longitude variables of
the site (\code{si_lat} and \code{si_long})}

\item{add_solar_ts}{Logical indicating if solar timestamp must be added to
the environmental data frame.}
}
\value{
A data frame exactly as \code{data}, but with an additional column
  containing the extraterrestrial radiation in W/m2, and optionally another
  column containing apparent solar timestamp.
}
\description{
Calculate the Extraterrestrial Radiation from the TIMESTAMP
}
\details{
This function uses several functions from \code{solaR} package in order to
obtain the mean solar time, the equation of time for each day included in the
TIMESTAMP and the extraterrestrial radiation for each step of the TIMESTAMP.
}
\section{Apparent (Real) Solar Time}{

The Apparent Solar Time is calculated as:
\deqn{Apparent Solar Time = Mean Solar Time + Equation of Time}
The Equation of Time is calculated for each day, whereas the Mean Solar Time
is calculated for each step of the TIMESTAMP.
}

\seealso{
Other Quality Checks Functions: \code{\link{create_dic}},
  \code{\link{qc_coordinates}},
  \code{\link{qc_data_results_table}},
  \code{\link{qc_env_ranges}},
  \code{\link{qc_env_vars_presence}},
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
