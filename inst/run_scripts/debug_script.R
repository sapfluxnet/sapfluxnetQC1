# Libraries
library(sapfluxnetQC1)
library(dplyr)
library(DT)

params <- list(
  wd = '../',
  md_file = 'foo.xlsx',
  sapf_data_file = 'foo.xlsx',
  env_data_file = 'foo.xlsx',
  code = 'foo'
)

logger_name <- 'test'

# Data load
## site_md
site_md <- dl_metadata(params$md_file, 'site_md', parent_logger = logger_name)

## stand_md
stand_md <- dl_metadata(params$md_file, 'stand_md', si_code_loc = site_md,
                        parent_logger = logger_name)

## species_md
species_md <- dl_metadata(params$md_file, 'species_md', si_code_loc = site_md,
                          parent_logger = logger_name)

## plant_md
plant_md <- dl_metadata(params$md_file, 'plant_md', si_code_loc = site_md,
                        parent_logger = logger_name)

## env_md
env_md <- dl_metadata(params$md_file, 'environmental_md', si_code_loc = site_md,
                      parent_logger = logger_name)

## sapf_data
sapf_data <- dl_data(params$sapf_data_file, 'sapflow_hd', n = 2000, na = '',
                     parent_logger = logger_name)

## env_data
env_data <- dl_data(params$env_data_file, 'environmental_hd', n = 2000, na = '',
                    parent_logger = logger_name)

################################################################################

# md qc

## metadata columns
md_cols <- bind_rows(
  qc_md_cols(site_md, 'site_md', parent_logger = logger_name),
  qc_md_cols(stand_md, 'stand_md', parent_logger = logger_name),
  qc_md_cols(species_md, 'species_md', parent_logger = logger_name),
  qc_md_cols(plant_md, 'plant_md', parent_logger = logger_name),
  qc_md_cols(env_md, 'environmental_md', parent_logger = logger_name)
)

## factor variables values
factor_values <- qc_factor_values(site_md, stand_md, species_md,
                                  plant_md, env_md, parent_logger = logger_name)

## email
email_check <- qc_email_check(site_md, parent_logger = logger_name)

## coordinates
site_md_coordfix <- qc_coordinates(site_md, parent_logger = logger_name)

## soil_texture
stand_md <- qc_soil_texture(stand_md, parent_logger = logger_name)

## species
species_md_spnames <- qc_species_names_info(
  species_md$sp_name,
  parent_logger = logger_name
) %>%
  mutate(Md = 'sp')

plant_md_spnames <- qc_species_names_info(
  plant_md$pl_species,
  parent_logger = logger_name
) %>%
  mutate(Md = 'pl')

species_md$sp_name <- qc_species_names(species_md$sp_name,
                                       parent_logger = logger_name)
plant_md$pl_species <- qc_species_names(plant_md$pl_species,
                                        parent_logger = logger_name)
sp_verification <- qc_species_verification(species_md, plant_md,
                                           parent_logger = logger_name)

## plant treatment check
pl_treatments_check <- qc_pl_treatments(plant_md, parent_logger = logger_name)

## environmental vars presence
env_var_presence <- qc_env_vars_presence(
  env_data, env_md, parent_logger = logger_name
)

################################################################################

# data qc
## timestamp
### sapf
sapf_data_fixed <- qc_fix_timestamp(sapf_data, env_md,  logger_name)
### env
env_data_fixed <- qc_fix_timestamp(env_data, env_md, logger_name)

## timestamp NAs
### sapf
sapf_timestamp_nas <- qc_timestamp_nas(sapf_data_fixed, logger_name)
### env
env_timestamp_nas <- qc_timestamp_nas(env_data_fixed, logger_name)

## timestamp errors
### sapf
timestamp_errors_sapf <- qc_timestamp_errors(
  sapf_data_fixed,
  timestep = qc_get_timestep(plant_md, parent_logger = logger_name),
  parent_logger = logger_name
)
### env
timestamp_errors_env <- qc_timestamp_errors(
  env_data_fixed,
  timestep = qc_get_timestep(env_md, parent_logger = logger_name),
  parent_logger = logger_name
)

## Timestamp concordance
timestamp_concordance <- qc_timestamp_concordance(
  sapf_data_fixed, env_data_fixed,
  plot = FALSE, parent_logger = logger_name
)
timestamp_concordance_plot <- qc_timestamp_concordance(
  sapf_data_fixed, env_data_fixed,
  plot = TRUE, parent_logger = logger_name
)

### concordance and gaps info
gap_lines_plot <- vis_gap_lines(sapf_data_fixed, env_data_fixed,
                                parent_logger = logger_name)

## Gaps
sapf_gaps_info <- qc_mind_the_gap(
  sapf_data_fixed, parent_logger = logger_name
)
env_gaps_info <- qc_mind_the_gap(
  env_data_fixed, parent_logger = logger_name
)

### plots
#### calendars
sapf_gaps_cal <- vis_gaps_calendar(sapf_data_fixed, parent_logger = logger_name)
env_gaps_cal <- vis_gaps_calendar(env_data_fixed, parent_logger = logger_name)

#### plot the gaps coverage
sapf_gaps_plot <- vis_plot_the_gap(sapf_gaps_info, type = "gap_coverage",
                                   parent_logger = logger_name)
env_gaps_plot <- vis_plot_the_gap(env_gaps_info, type = "gap_coverage",
                                  parent_logger = logger_name)

#### plot the gaps interval
sapf_gaps_plot_int <- vis_plot_the_gap(sapf_gaps_info, type = "gap_interval",
                                       parent_logger = logger_name)
env_gaps_plot_int <- vis_plot_the_gap(env_gaps_info, type = "gap_interval",
                                      parent_logger = logger_name)

################################################################################
# create the SfnData object and save it as a RData file for later use
## sfndata_object
sfn_data_object <- sfn_data_constructor(
  sapf_data = sapf_data_fixed, env_data = env_data_fixed,
  site_md = site_md_coordfix, stand_md = stand_md, plant_md = plant_md,
  species_md = species_md, env_md = env_md,
  parent_logger = logger_name
)

# save it!
assign(params$code, sfn_data_object)
save(list = c(params$code),
     file = file.path('Data', params$code, 'Lvl_1',
                      paste(params$code, '.RData', sep = '')),
     envir = environment())

################################################################################
# trasnformations availabilty
transformations_table <- qc_transformation_vars(
  sfn_data_object, parent_logger = logger_name
) %>%
  qc_transf_list(parent_logger = logger_name)

################################################################################
# results md_qc table
qc_md_results_table(md_cols, factor_values, email_check, site_md_coordfix,
                    species_md, plant_md, species_md_spnames, plant_md_spnames,
                    sp_verification, env_var_presence,
                    parent_logger = logger_name)
################################################################################
################################################################################
# table
qc_data_results_table(sapf_data_fixed, env_data_fixed, timestamp_errors_sapf,
                      timestamp_errors_env, sapw_md,
                      timestamp_concordance, sapf_gaps_info,
                      env_gaps_info, sapf_timestamp_nas, env_timestamp_nas,
                      parent_logger = logger_name)
################################################################################

# 2.2.6 saving the fixed datasets and the objects created in the level1 folder
df_accepted_to_lvl1(
  params$code, sapf_data_fixed, env_data_fixed,
  site_md_coordfix, stand_md, plant_md, species_md,
  env_md, parent_logger = 'DataFlow'
)

# saving Rdata file with all the objects (just in case)
save(list = ls(all.names = TRUE),
     file = file.path('Data', params$code, 'Lvl_1',
                      paste(params$code, 'objects.RData', sep = '_')),
     envir = environment())
