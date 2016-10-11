library(sapfluxnetr)

################################################################################
context('O1. SfnData Class')



################################################################################
# cleaning
unlink('FakeData', recursive = TRUE)
unlink('received_data', recursive = TRUE)
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)
unlink('ESP_adm0.rds')
