library(sapfluxnetr)
library(tools)

################################################################################
context('J1. Server tree folder')

df_folder_structure()

test_that('folders are created correctly', {
  expect_true(all(dir.exists(c('Data', 'Logs', 'Reports', 'Templates'))))
})

test_that('warnings are raised if folder already exist', {
  expect_warning(df_folder_structure(), './Data')
  expect_warning(df_folder_structure(), './Logs')
  expect_warning(df_folder_structure(), './Reports')
  expect_warning(df_folder_structure(), './Templates')
})

################################################################################
context('J2. received_to_accepted')

dir.create('received_data')
file.copy(from = c('foo.csv', 'foo.xlsx', 'foo_env.csv'),
          to = c('received_data/foo_sapflow_data.csv',
                 'received_data/foo_metadata.xlsx',
                 'received_data/foo_env_data.csv'))

suppressMessages(df_received_to_accepted(remove = FALSE))

received_md5 <- md5sum(c('received_data/foo_sapflow_data.csv',
                         'received_data/foo_metadata.xlsx',
                         'received_data/foo_env_data.csv'))
accepted_md5 <- md5sum(c('Data/foo/Accepted/foo_sapflow_data.csv',
                          'Data/foo/Accepted/foo_metadata.xlsx',
                          'Data/foo/Accepted/foo_env_data.csv'))

test_that('md5sums are correct', {
  expect_true(all(received_md5 == accepted_md5))
})

test_that('warnings and messages are raised correctly', {
  expect_warning(df_received_to_accepted(), 'site is already in the system')
  expect_warning(df_received_to_accepted(),
                 'Not copying any file, manual intervention needed')
  # remove the files to see other warnings
  unlink('Data/foo', recursive = TRUE)

  expect_warning(df_received_to_accepted(),
                 'remove argument set to FALSE, data will be left')

  # remove the files to see other warnings
  unlink('Data/foo', recursive = TRUE)

  expect_message(df_received_to_accepted(remove = TRUE),
                 'Removing the received files for site')
  expect_false(file.exists('received_data/foo_sapflow_data.csv'))
  expect_false(file.exists('received_data/foo_env_data.csv'))
  expect_false(file.exists('received_data/foo_metadata.xlsx'))
})

file.copy(from = c('foo.csv', 'foo.xlsx', 'foo_env.csv'),
          to = c('received_data/bar_sapflow.csv',
                 'received_data/bar.xlsx',
                 'received_data/bar_environmental.csv'))

suppressMessages(df_received_to_accepted(remove = FALSE))

test_that('bar site files are not copied', {
  expect_false(dir.exists('Data/bar'))
  expect_false(file.exists('Data/bar/Accepted/bar.xlsx'))
  expect_false(file.exists('Data/bar/Accepted/bar_sapflow.csv'))
  expect_false(file.exists('Data/bar/Accepted/bar_environmental.csv'))
})

unlink('received_data', recursive = TRUE)
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)

################################################################################
context('J3. Status yaml files')

df_folder_structure()

dir.create('received_data')

file.copy(from = c('foo.csv', 'foo.xlsx', 'foo_env.csv'),
          to = c('received_data/foo_sapflow_data.csv',
                 'received_data/foo_metadata.xlsx',
                 'received_data/foo_env_data.csv'))

suppressMessages(df_received_to_accepted(remove = FALSE))

test_that('status file functions work', {
  expect_true(df_start_status('foo'))

  foo_yaml <- df_get_status('foo')

  expect_true(file.exists('Data/foo/foo_status.yaml'))
  expect_is(foo_yaml, 'list')
  expect_false(foo_yaml$QC$DONE)
  expect_false(foo_yaml$LVL1$STORED)
  expect_false(foo_yaml$LVL2$STORED)
  expect_true(is.null(foo_yaml$QC$DATE))
  expect_true(is.null(foo_yaml$LVL1$DATE))
  expect_true(is.null(foo_yaml$LVL2$DATE))

  expect_true(df_set_status('foo', QC = list(DONE = TRUE,
                                             DATE = as.character(Sys.Date()))))

  foo_yaml <- df_get_status('foo')

  expect_true(foo_yaml$QC$DONE)
  expect_is(foo_yaml$QC$DATE, 'character')
  expect_false(foo_yaml$LVL1$STORED)
  expect_false(foo_yaml$LVL2$STORED)
  expect_true(is.null(foo_yaml$LVL1$DATE))
  expect_true(is.null(foo_yaml$LVL2$DATE))

})

test_that('messages are correctly raised', {
  expect_warning(df_start_status('foo'),
                 'already exists, skipping creation')
  expect_false(suppressWarnings(df_start_status('foo')))

  expect_warning(df_get_status('bar'), 'does not exist, unable to retrieve info')
  expect_false(suppressWarnings(df_get_status('bar')))

  expect_warning(df_set_status('bar', QC = list(DONE = TRUE,
                                                DATE = as.character(Sys.Date()))),
                 'does not exist, unable to retrieve info')
  expect_false(suppressWarnings(df_set_status('bar', QC = list(DONE = TRUE,
                                              DATE = as.character(Sys.Date())))))
})

################################################################################
context('J4. Report folder tree creation')

code <- 'foo'

test_that('argument checks work', {
  expect_error(df_report_folder_creation(vector()),
               'si_code provided is not a character')
  expect_error(df_report_folder_creation(5),
               'si_code provided is not a character')
})

df_report_folder_creation(code)

test_that('folder is created', {
  expect_true(dir.exists(file.path('Reports', 'foo')))
})

test_that('if folder exists it is detected', {
  expect_false(df_report_folder_creation(code))
  expect_message(df_report_folder_creation(code), 'folder already exists')
})

################################################################################
context('J5. Getting the data folders')

test_that('function works', {
  expect_is(df_get_data_folders(), 'character')
  expect_length(df_get_data_folders(), 1)
  expect_identical(df_get_data_folders()[1], 'Data/foo')
})

dir.create('Data/bar')

test_that('function works', {
  expect_is(df_get_data_folders(), 'character')
  expect_length(df_get_data_folders(), 2)
  expect_identical(df_get_data_folders(), c('Data/bar', 'Data/foo'))
})

################################################################################
context('J6. Accepted to level 1 data flow')

code <- 'legen_wait_for_it'
sapf_data_plant <- data.frame(a = letters[c(4,1,18,25)],
                              b = rnorm(4))
sapf_data_sapwood <- data.frame(a = letters[c(4,1,18,25)],
                              b = rnorm(4)*10)
sapf_data_leaf <- data.frame(a = letters[c(4,1,18,25)],
                              b = rnorm(4))
env_data <- data.frame(a = letters[c(4,1,18,25)],
                       b = rnorm(4),
                       c = rnorm(4))
site_md <- data.frame(a = letters[c(4,1,18,25)],
                       b = rnorm(4))
stand_md <- data.frame(a = letters[c(4,1,18,25)],
                      b = rnorm(4))
plant_md <- data.frame(a = letters[c(4,1,18,25)],
                      b = rnorm(4))
species_md <- data.frame(a = letters[c(4,1,18,25)],
                      b = rnorm(4))
env_md <- data.frame(a = letters[c(4,1,18,25)],
                      b = rnorm(4))
fake_env_data <- vector()

dir.create(file.path('Data', code))
dir.create(file.path('Data', code, 'Accepted'))
dir.create(file.path('Data', code, 'Lvl_1'))
df_report_folder_creation(code)
df_start_status(code)
df_set_status(code, QC = list(DONE = TRUE, DATE = Sys.Date()))

md_cols <- 1
factor_values <- 2
email_check <- 3
species_md_spnames <- 4
plant_md_spnames <- 5
sp_verification <- 6
pl_treatments_check <- 7
env_var_presence <- 8
timestamp_errors_sapf <- 9
timestamp_errors_env <- 10
timestamp_concordance <- 11
timestamp_concordance_plot <- 12
gap_lines_plot <- 13
sapf_gaps_info <- 14
env_gaps_info <- 15
sapf_gaps_trim_info <- 16
env_gaps_trim_info <- 17
sapf_gaps_cal <- 18
env_gaps_cal <- 19
sapf_gaps_plot <- 20
env_gaps_plot <- 21
sapf_gaps_trim_plot <- 22
env_gaps_trim_plot <- 23
sapf_gaps_plot_int <- 24
env_gaps_plot_int <- 25
sapf_gaps_trim_plot_int <- 26
env_gaps_trim_plot_int <- 27
sapw_md <- 28

test_that('argument checks work', {
  expect_error(df_accepted_to_lvl1(si_code = code, sapf_data_plant = sapf_data_plant,
                                   env_data = env_data, site_md = site_md,
                                   stand_md = stand_md, plant_md = plant_md,
                                   species_md = species_md),
               'One or more datasets were not provided')
  expect_error(df_accepted_to_lvl1(si_code = code, sapf_data_plant = sapf_data_plant,
                                   env_data = env_data, site_md = site_md,
                                   env_md = env_md, plant_md = plant_md,
                                   species_md = species_md),
               'One or more datasets were not provided')
  expect_error(df_accepted_to_lvl1(si_code = code, sapf_data_sapwood = sapf_data_sapwood,
                                   env_data = fake_env_data, site_md = site_md,
                                   stand_md = stand_md, plant_md = plant_md,
                                   species_md = species_md, env_md = env_md),
               'One or more datasets provided are not data frames')
  expect_error(df_accepted_to_lvl1(si_code = 3, sapf_data_plant = sapf_data_plant,
                                   env_data = env_data, site_md = site_md,
                                   stand_md = stand_md, plant_md = plant_md,
                                   species_md = species_md, env_md = env_md),
               'site code provided is not a character string')
})

test_that('files are missing before the function action', {
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'sapflow_data_plant.csv',
                                           sep = '_'))))
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'sapflow_data_sapwood.csv',
                                           sep = '_'))))
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'sapflow_data_leaf.csv',
                                           sep = '_'))))
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'site_md.csv',
                                           sep = '_'))))
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'stand_md.csv',
                                           sep = '_'))))
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'plant_md.csv',
                                           sep = '_'))))
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'species_md.csv',
                                           sep = '_'))))
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'env_md.csv',
                                           sep = '_'))))
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'objects.RData',
                                           sep = '_'))))
})

dary_yaml <- df_get_status(code)

test_that('status file LVL1 is empty before the function action', {
  expect_false(dary_yaml$LVL1$STORED)
  expect_null(dary_yaml$LVL1$DATE)
})

df_accepted_to_lvl1(code, sapf_data_plant, sapf_data_sapwood,
                    sapf_data_leaf, env_data, site_md,
                    stand_md, plant_md, species_md, env_md,
                    rdata = FALSE)

test_that('files were created', {
  expect_true(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'sapflow_data_plant.csv',
                                           sep = '_'))))
  expect_true(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'sapflow_data_sapwood.csv',
                                           sep = '_'))))
  expect_true(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'sapflow_data_leaf.csv',
                                           sep = '_'))))
  expect_true(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'site_md.csv',
                                           sep = '_'))))
  expect_true(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'stand_md.csv',
                                           sep = '_'))))
  expect_true(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'plant_md.csv',
                                           sep = '_'))))
  expect_true(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'species_md.csv',
                                           sep = '_'))))
  expect_true(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'env_md.csv',
                                           sep = '_'))))
  # expect_true(file.exists(file.path('Data', code, 'Lvl_1',
  #                                    paste(code, 'objects.RData',
  #                                          sep = '_'))))
})

dary_yaml <- df_get_status(code)

test_that('status file has been updated', {
  expect_true(dary_yaml$LVL1$STORED)
  expect_equal(dary_yaml$LVL1$DATE, as.character(Sys.Date()))
})

rm(list = ls())

test_that('no objects in environment now', {
  expect_length(ls(), 0)
})

# NOTE: I can't test the RData generation as environments are a little wobbly in
# tests. Looking for a solution to this.

# load(file.path('Data', 'legen_wait_for_it', 'Lvl_1',
#                paste('legen_wait_for_it', 'objects.RData',
#                      sep = '_')),
#      envir = test_env())
#
# test_that('objects from RData file are correctly created when loading file', {
#   expect_length(ls(), 28)
#   expect_identical(ls(), c('email_check', 'env_gaps_cal',
#                            'env_gaps_info', 'env_gaps_plot', 'env_gaps_plot_int',
#                            'env_gaps_trim_info', 'env_gaps_trim_plot',
#                            'env_gaps_trim_plot_int', 'env_var_presence',
#                            'factor_values', 'gap_lines_plot', 'md_cols',
#                            'plant_md_spnames', 'pl_treatments_check',
#                            'sapf_gaps_cal', 'sapf_gaps_info', 'sapf_gaps_plot',
#                            'sapf_gaps_plot_int', 'sapf_gaps_trim_info',
#                            'sapf_gaps_trim_plot', 'sapf_gaps_trim_plot_int',
#                            'sapw_md', 'species_md_spnames', 'sp_verification',
#                            'timestamp_concordance', 'timestamp_concordance_plot',
#                            'timestamp_errors_env', 'timestamp_errors_sapf'))
# })

################################################################################
# cleaning
unlink('received_data', recursive = TRUE)
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)



