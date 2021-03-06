% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_main.R
\name{qc_md_results_table}
\alias{qc_md_results_table}
\title{Main function to resume Metadata QC in one data frame}
\usage{
qc_md_results_table(md_cols, factor_values, email_check, site_md_coordfix,
  species_md, plant_md, species_md_spnames, plant_md_spnames, sp_verification,
  env_var_presence, parent_logger = "test")
}
\arguments{
\item{md_cols}{}

\item{factor_values}{}

\item{email_check}{}

\item{site_md_coordfix}{}

\item{species_md}{}

\item{plant_md}{}

\item{species_md_spnames}{}

\item{plant_md_spnames}{}

\item{sp_verification}{}

\item{env_var_presence}{}
}
\value{
A data frame with the highlights of the QC
}
\description{
Metadata QC codified results in one data frame
}
\seealso{
Other Quality Checks Functions: \code{\link{create_dic}},
  \code{\link{qc_coordinates}},
  \code{\link{qc_data_results_table}},
  \code{\link{qc_env_ranges}},
  \code{\link{qc_env_vars_presence}},
  \code{\link{qc_ext_radiation}},
  \code{\link{qc_factor_values}},
  \code{\link{qc_fix_timestamp}},
  \code{\link{qc_get_biomes_spdf}},
  \code{\link{qc_get_biome}}, \code{\link{qc_get_sapw_md}},
  \code{\link{qc_get_timestep}},
  \code{\link{qc_get_timezone}},
  \code{\link{qc_is_timestamp}}, \code{\link{qc_md_cols}},
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
