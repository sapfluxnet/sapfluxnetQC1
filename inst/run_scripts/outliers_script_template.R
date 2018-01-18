
################# Template starts here #########################################
# Objects obtained from the app
# code: site code as character
# to_remove: list containing the variables as labels and the rows selected to
#            outlier remove as values

# 0. setup

## 0.1 libraries
require(sapfluxnetQC1)

## 0.2 log
log_sapfluxnet_setup('Logs/sapfluxnet.log',
                     logger = 'Outliers_remove',
                     level = 'DEBUG')

# 1. get data
load(file = file.path('Data', 'Lvl_2', 'lvl2_out_warn', paste0(code, '.RData')))

sfn_data <- eval(as.name(code))

# 2. process outliers (first step, complete)
sfn_data_out_rem <- out_remove(sfn_data, substitute = TRUE,
                               parent_logger = 'Outliers_remove')

# 3. get the stored values and the original values
sapf_data <- get_sapf(sfn_data)
sapf_data_out_rem <- get_sapf(sfn_data_out_rem)
env_data <- get_env(sfn_data)
env_data_out_rem <- get_env(sfn_data_out_rem)

# 4. substitute the desired outliers
