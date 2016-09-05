# Main script

library(sapfluxnetr)

################################################################################
# server preparation, run only once!!!!!!!
################################################################################

# # folder structure
# df_folder_structure(parent_logger = 'DataFlow')
# 
# # Copy templates to Template folder
# file.copy(
#   system.file("Rmd_templates", "received_to_accepted.Rmd",
#               package = "sapfluxnetr"),
#   file.path('Templates')
# )
# file.copy(
#   system.file("Rmd_templates", "QC_report.Rmd",
#               package = "sapfluxnetr"),
#   file.path('Templates')
# )

################################################################################
# END server preparation
################################################################################

# setup logs
log_sapfluxnet_setup('Logs/sapfluxnet.log',
                     logger = 'DataFlow',
                     level = 'WARNING')

# reports for data in the system
rep_sfn_render('received_to_accepted.Rmd',
               output_file = file.path(
                 'Reports', paste(format(Sys.time(), '%Y%m%d%H%M'),
                                  'received_to_accepted.html', sep = '_')
               ),
               output_dir = 'Reports',
               parent_logger = 'DataFlow')

# QC
log_sapfluxnet_setup('Logs/sapfluxnet.log', logger = 'QC', level = "WARNING")

data_folders <- df_get_data_folders(parent_logger = 'QC')

for (folder in data_folders) {
  log_sapfluxnet_setup('Logs/sapfluxnet.log', logger = paste('QC', folder, sep = '_'),
                       level = "WARNING")
  qc_start_process(file.path(folder, 'Accepted'), rdata = FALSE,
                   parent_logger = paste('QC', folder, sep = '_')) 
}
