# Main script

library(sapfluxnetQC1)

################################################################################
# server preparation, run only once!!!!!!!
################################################################################

# folder structure
# df_folder_structure(parent_logger = 'DataFlow')

# Copy templates to Template folder
# df_copy_templates()

################################################################################
# END server preparation
################################################################################

################################################################################
# server management run when necessary
################################################################################

# log_sapfluxnet_setup('Logs/sapfluxnet.log',
#                      logger = 'Server_Management',
#                      level = "WARNING")
#
# df_get_data_folders() %>%
#   stringr::str_sub(6, -1) %>%
#   purrr::walk(sm_status_updater, parent_logger = 'Server_Management') %>%
#   purrr::walk(sm_solarTIMESTAMP_adder, parent_logger = 'Server_Management')

################################################################################
# END server management
################################################################################


# setup logs
log_sapfluxnet_setup('Logs/sapfluxnet.log',
                     logger = 'DataFlow',
                     level = 'DEBUG')

# reports for data in the system
rep_sfn_render('received_to_accepted.Rmd',
               output_file = file.path(
                 'Reports', paste(format(Sys.time(), '%Y%m%d%H%M'),
                                  'received_to_accepted.html', sep = '_')
               ),
               output_dir = 'Reports',
               parent_logger = 'DataFlow')

# QC
log_sapfluxnet_setup('Logs/sapfluxnet.log', logger = 'QC', level = "DEBUG")

data_folders <- df_get_data_folders(parent_logger = 'QC')

## Loop for every site
lapply(data_folders, function(folder) {
  code <- stringr::str_sub(folder, 6, -1)
  # log_sapfluxnet_setup('Logs/sapfluxnet.log',
  #                      logger = paste('QC', code, sep = '.'),
  #                      level = "DEBUG")
  qc_start_process(file.path(folder, 'Accepted'), rdata = FALSE,
                   parent_logger = paste('QC', code, sep = '.'))
})

################################################################################
# LEVEL 2

log_sapfluxnet_setup('Logs/sapfluxnet.log', logger = 'LEVEL2', level = "DEBUG")

df_lvl1_to_lvl2(parent_logger = 'LEVEL2')

########

# out_app()

df_warn_to_rem()

# out_confirmation_app()

df_rem_to_units()
