################################################################################
# libraries
library(sapfluxnetr)
library(dplyr)
# library(DT)
# library(dygraphs)
# library(xts)
################################################################################
# data load
log_sapfluxnet_setup('Logs/sapfluxnet.log', 'data_load')

## site_md
site_md <- dl_metadata('Data/AUS_MAR_MSD_MOD/Accepted/AUS_MAR_MSD_MOD_metadata.xlsx', 'site_md', parent_logger = 'data_load')

## stand_md
stand_md <- dl_metadata('Data/AUS_MAR_MSD_MOD/Accepted/AUS_MAR_MSD_MOD_metadata.xlsx', 'stand_md', si_code_loc = site_md, parent_logger = 'data_load')

## species_md
species_md <- dl_metadata('Data/AUS_MAR_MSD_MOD/Accepted/AUS_MAR_MSD_MOD_metadata.xlsx', 'species_md', si_code_loc = site_md, parent_logger = 'data_load')

## plant_md
plant_md <- dl_metadata('Data/AUS_MAR_MSD_MOD/Accepted/AUS_MAR_MSD_MOD_metadata.xlsx', 'plant_md', si_code_loc = site_md, parent_logger = 'data_load')

## env_md
env_md <- dl_metadata('Data/AUS_MAR_MSD_MOD/Accepted/AUS_MAR_MSD_MOD_metadata.xlsx', 'environmental_md', si_code_loc = site_md, parent_logger = 'data_load')

## sapf_data
sapf_data <- dl_data('Data/AUS_MAR_MSD_MOD/Accepted/AUS_MAR_MSD_MOD_metadata.xlsx', 'sapflow_hd',
                     n = 2000, parent_logger = 'data_load')

## env_data
env_data <- dl_data('Data/AUS_MAR_MSD_MOD/Accepted/AUS_MAR_MSD_MOD_metadata.xlsx', 'environmental_hd',
                    n = 2000, parent_logger = 'data_load')

################################################################################
# md qc
log_sapfluxnet_setup('Logs/sapfluxnet.log', 'md_qc')

## metadata columns
md_cols <- bind_rows(
  qc_md_cols(site_md, 'site_md', parent_logger = 'md_qc'),
  qc_md_cols(stand_md, 'stand_md', parent_logger = 'md_qc'),
  qc_md_cols(species_md, 'species_md', parent_logger = 'md_qc'),
  qc_md_cols(plant_md, 'plant_md', parent_logger = 'md_qc'),
  qc_md_cols(env_md, 'environmental_md', parent_logger = 'md_qc')
)

## factor variables values 
factor_values <- qc_factor_values(site_md, stand_md, species_md, plant_md, env_md,
                                  parent_logger = 'md_qc')

## email
email_check <- qc_email_check(site_md, parent_logger = 'md_qc')

## coordinates
# site_md_coordfix <-
#   site_md %T>%
#   qc_download_maps(parent_logger = 'md_qc') %>%
#   as.data.frame() %>%
#   qc_check_coordinates(parent_logger = 'md_qc') %>%
#   qc_fix_latlong_errors(special_countries = TRUE, parent_logger = 'md_qc')

site_md_coordfix <- qc_coordinates(site_md, parent_logger = 'md_qc')

## species
species_md_spnames <- qc_species_names_info(species_md$sp_name,
                                            parent_logger = 'md_qc') %>%
  mutate(Md = 'sp')
plant_md_spnames <- qc_species_names_info(plant_md$pl_species,
                                          parent_logger = 'md_qc') %>%
  mutate(Md = 'pl')


species_md$sp_name <- qc_species_names(species_md$sp_name, parent_logger = 'md_qc')
plant_md$pl_species <- qc_species_names(plant_md$pl_species, parent_logger = 'md_qc')
sp_verification <- qc_species_verification(species_md, plant_md, parent_logger = 'md_qc')

## plant treatment check
pl_treatments_check <- qc_pl_treatments(plant_md, parent_logger = 'md_qc')

## environmental vars presence
env_var_presence <- qc_env_vars_presence(env_data, env_md, parent_logger = 'md_qc')

################################################################################
# table
# qc_md_results_table(md_cols, factor_values, email_check, site_md_coordfix,
#                     species_md, plant_md, species_md_spnames, plant_md_spnames,
#                     sp_verification, env_var_presence, 'md_qc')
################################################################################

################################################################################
# data qc
log_sapfluxnet_setup('Logs/sapfluxnet.log', 'data_qc')

## timestamp
### sapf
sapf_data_fixed <- qc_fix_timestamp(sapf_data, env_md,  'data_qc')
### env
env_data_fixed <- qc_fix_timestamp(env_data, env_md, 'data_qc')

## timestamp NAs
### sapf
sapf_timestamp_nas <- qc_timestamp_nas(sapf_data_fixed, 'data_qc')
### env
env_timestamp_nas <- qc_timestamp_nas(env_data_fixed, 'data_qc')

## timestamp errors
### sapf
timestamp_errors_sapf <- qc_timestamp_errors(
  sapf_data_fixed,
  timestep = qc_get_timestep(plant_md, parent_logger = 'data_qc'),
  parent_logger = 'data_qc'
)
### env
timestamp_errors_env <- qc_timestamp_errors(
  env_data_fixed,
  timestep = qc_get_timestep(env_md, parent_logger = 'data_qc'),
  parent_logger = 'data_qc'
)

## Timestamp concordance
timestamp_concordance <- qc_timestamp_concordance(sapf_data_fixed, env_data_fixed,
                                                  plot = FALSE, parent_logger = 'data_qc')
timestamp_concordance_plot <- qc_timestamp_concordance(sapf_data_fixed, env_data_fixed,
                                                       plot = TRUE, parent_logger = 'data_qc')

### concordance and gaps info
gap_lines_plot <- vis_gap_lines(sapf_data_fixed, env_data_fixed, parent_logger = 'data_qc')

## Gaps
### No trimmed
sapf_gaps_info <- qc_mind_the_gap(sapf_data_fixed, parent_logger = 'data_qc')
env_gaps_info <- qc_mind_the_gap(env_data_fixed, parent_logger = 'data_qc')
### Trimmed
sapf_gaps_trim_info <- qc_mind_the_gap(sapf_data_fixed, trim = TRUE, parent_logger = 'data_qc')
env_gaps_trim_info <- qc_mind_the_gap(env_data_fixed, trim = TRUE, parent_logger = 'data_qc')

### plots
#### calendars
sapf_gaps_cal <- vis_gaps_calendar(sapf_data_fixed, parent_logger = 'data_qc')
env_gaps_cal <- vis_gaps_calendar(env_data_fixed, parent_logger = 'data_qc')
#### plot the gaps coverage
sapf_gaps_plot <- vis_plot_the_gap(sapf_gaps_info, type = "gap_coverage",
                                   parent_logger = 'data_qc')
env_gaps_plot <- vis_plot_the_gap(env_gaps_info, type = "gap_coverage",
                                  parent_logger = 'data_qc')
sapf_gaps_trim_plot <- vis_plot_the_gap(sapf_gaps_trim_info, type = "gap_coverage",
                                        parent_logger = 'data_qc')
env_gaps_trim_plot <- vis_plot_the_gap(env_gaps_trim_info, type = "gap_coverage",
                                       parent_logger = 'data_qc')
#### plot the gaps interval
sapf_gaps_plot_int <- vis_plot_the_gap(sapf_gaps_info, type = "gap_interval",
                                       parent_logger = 'data_qc')
env_gaps_plot_int <- vis_plot_the_gap(env_gaps_info, type = "gap_interval",
                                      parent_logger = 'data_qc')
sapf_gaps_trim_plot_int <- vis_plot_the_gap(sapf_gaps_trim_info, type = "gap_interval",
                                            parent_logger = 'data_qc')
env_gaps_trim_plot_int <- vis_plot_the_gap(env_gaps_trim_info, type = "gap_interval",
                                           parent_logger = 'data_qc')

## Unit conversion
sapw_md <- qc_sapw_area_calculator(qc_get_sapw_md(plant_md,
                                                  parent_logger = 'data_qc'),
                                   parent_logger = 'data_qc')

### plant units
sapf_data_fixed_plant <- qc_sapw_conversion(sapf_data_fixed, sapw_md,
                                            output_units = 'plant',
                                            parent_logger = 'data_qc')

#### check if errors
sapf_data_fixed_plant <- tryCatch(sapf_data_fixed_plant, error = function(e) return(NULL))

### sapwood units
sapf_data_fixed_sapwood <- qc_sapw_conversion(sapf_data_fixed, sapw_md,
                                              output_units = 'sapwood',
                                              parent_logger = 'data_qc')

#### check if errors
sapf_data_fixed_sapwood <- tryCatch(sapf_data_fixed_sapwood, error = function(e) return(NULL))

### leaf units
sapf_data_fixed_leaf <- qc_sapw_conversion(sapf_data_fixed, sapw_md,
                                           output_units = 'leaf',
                                           parent_logger = 'data_qc')

#### check if errors
sapf_data_fixed_leaf <- tryCatch(sapf_data_fixed_leaf, error = function(e) return(NULL))


################################################################################
# table
# qc_data_results_table(sapf_data_fixed, env_data_fixed, timestamp_errors_sapf,
#                       timestamp_errors_env, sapw_md,
#                       timestamp_concordance, sapf_gaps_info,
#                       env_gaps_info, sapf_data_fixed_plant,
#                       sapf_data_fixed_sapwood, sapf_data_fixed_leaf,
#                       sapf_timestamp_nas, env_timestamp_nas,
#                       'data_qc')
################################################################################

# df_accepted_to_lvl1('AUS_MAR_MSD_MOD', sapf_data_fixed_plant, sapf_data_fixed_sapwood,
#                     sapf_data_fixed_leaf, env_data_fixed, site_md_coordfix,
#                     stand_md, plant_md, species_md, env_md, rdata = FALSE,
#                     parent_logger = 'DataFlow')
