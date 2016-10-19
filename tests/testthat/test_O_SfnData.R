library(sapfluxnetr)

################################################################################
context('O1. SfnData Class')

# load foo objects
load('foo_objects.RData')

# build the SfnData object with foo_data
foo_sfndata <- SfnData(
  sapf_data = sapf_data_fixed[, -1],
  env_data = env_data_fixed[, -1],
  sapf_flags = data.frame(),
  env_flags = data.frame(),
  timestamp = sapf_data_fixed[[1]],
  si_code = site_md$si_code,
  site_md = site_md_coordfix,
  stand_md = stand_md,
  species_md = species_md_spnames,
  plant_md = plant_md_spnames,
  env_md = env_md
)

################################################################################
# cleaning
# unlink('FakeData', recursive = TRUE)
# unlink('received_data', recursive = TRUE)
# unlink('Data', recursive = TRUE)
# unlink('Logs', recursive = TRUE)
# unlink('Reports', recursive = TRUE)
# unlink('Templates', recursive = TRUE)
# unlink('ESP_adm0.rds')


