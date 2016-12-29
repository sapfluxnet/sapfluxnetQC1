library(sapfluxnetQC1)
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

suppressWarnings(suppressMessages(df_received_to_accepted(remove = FALSE)))

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

suppressWarnings(suppressMessages(df_received_to_accepted(remove = FALSE)))

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
sapf_data <- data.frame(a = letters[c(4,1,18,25)],
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
df_set_status(code, QC = list(DONE = TRUE, DATE = as.character(Sys.Date())))

test_that('argument checks work', {
  expect_error(df_accepted_to_lvl1(si_code = code, sapf_data = sapf_data,
                                   env_data = env_data, site_md = site_md,
                                   stand_md = stand_md, plant_md = plant_md,
                                   species_md = species_md),
               'One or more datasets were not provided')
  expect_error(df_accepted_to_lvl1(si_code = code, sapf_data = sapf_data,
                                   env_data = env_data, site_md = site_md,
                                   env_md = env_md, plant_md = plant_md,
                                   species_md = species_md),
               'One or more datasets were not provided')
  expect_error(df_accepted_to_lvl1(si_code = code, sapf_data = sapf_data,
                                   env_data = fake_env_data, site_md = site_md,
                                   stand_md = stand_md, plant_md = plant_md,
                                   species_md = species_md, env_md = env_md),
               'One or more datasets provided are not data frames')
  expect_error(df_accepted_to_lvl1(si_code = 3, sapf_data = sapf_data,
                                   env_data = env_data, site_md = site_md,
                                   stand_md = stand_md, plant_md = plant_md,
                                   species_md = species_md, env_md = env_md),
               'site code provided is not a character string')
})

test_that('files are missing before the function action', {
  expect_false(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'sapflow_data.csv',
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

df_accepted_to_lvl1(code, sapf_data, env_data, site_md,
                    stand_md, plant_md, species_md, env_md)

test_that('files were created', {
  expect_true(file.exists(file.path('Data', code, 'Lvl_1',
                                     paste(code, 'sapf_data.csv',
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

################################################################################

context('J7. Reset data and status')

code <- 'legen_wait_for_it'

old_yaml <- df_get_status(code)
old_files <- c(list.files(file.path('Data', code, 'Accepted'), full.names = TRUE),
               list.files(file.path('Data', code, 'Lvl_1'), full.names = TRUE))

df_reset_data_status(code)

new_yaml <- df_get_status(code)
new_files <- c(list.files(file.path('Data', code, 'Accepted'), full.names = TRUE),
               list.files(file.path('Data', code, 'Lvl_1'), full.names = TRUE))

test_that('status file has been updated', {
  expect_true(old_yaml$QC$DONE)
  expect_false(new_yaml$QC$DONE)
  expect_equal(old_yaml$QC$DATE, as.character(Sys.Date()))
  expect_null(new_yaml$QC$DATE)
  expect_true(old_yaml$LVL1$STORED)
  expect_false(new_yaml$LVL1$STORED)
  expect_equal(old_yaml$LVL1$DATE, as.character(Sys.Date()))
  expect_null(new_yaml$LVL1$DATE)
})

test_that('files had been renamed correctly', {
  expect_false(all(old_files == new_files))
  expect_true(length(old_files) == length(new_files))
  expect_match(new_files, "(.bak)")
  expect_match(old_files, "(.csv|.RData|.xlsx)")
})

################################################################################
context('J8. Saving templates and running scripts to folders')

unlink('Templates', recursive = TRUE)

test_that('Templates directory exists', {
  expect_error(df_copy_templates(),'Templates directory does not exist')
})

dir.create('Templates')

test_that('function works', {
  expect_error(df_copy_templates(),'Check whether templates and running scripts already exist.')
  expect_true(df_copy_templates(first = TRUE))
  expect_equal(dir('Templates'),c('QC_report.Rmd','received_to_accepted.Rmd'))
  expect_true(all(c('sfn_monitor.Rmd','main_script.R','debug_script.R') %in% dir('.')))
  expect_true(df_copy_templates())
})

################################################################################
context('J9. who is ready to level 2?')

# preparing the data folder
unlink('Data', recursive = TRUE)

dir.create(file.path('Data'))
dir.create(file.path('Data', 'foo'))
dir.create(file.path('Data', 'bar'))
dir.create(file.path('Data', 'baz'))

# creating empty statuses
df_start_status('foo')
df_start_status('bar')
df_start_status('baz')

# populate the statuses
df_set_status(
  'foo',
  QC = list(DONE = TRUE, DATE = Sys.Date()),
  LVL1 = list(STORED = TRUE, DATE = Sys.Date(), TO_LVL2 = TRUE)
)

df_set_status(
  'bar',
  QC = list(DONE = TRUE, DATE = Sys.Date()),
  LVL1 = list(STORED = TRUE, DATE = Sys.Date(), TO_LVL2 = FALSE)
)

df_set_status(
  'baz',
  QC = list(DONE = TRUE, DATE = Sys.Date()),
  LVL1 = list(STORED = TRUE, DATE = Sys.Date(), TO_LVL2 = NA)
)

# testing if function works
whos_ready <- df_who_ready_to_lvl2()

test_that('results are as expected',{
  expect_is(whos_ready, 'logical')
  expect_length(whos_ready, 3)
  expect_true(whos_ready['foo'])
  expect_false(whos_ready['bar'])
  expect_true(is.na(whos_ready['baz']))
})

# testing if empty
unlink(file.path('Data', 'bar', 'bar_status.yaml'))

whos_ready_2 <- suppressWarnings(df_who_ready_to_lvl2())

test_that('if any is null there is an error', {
  expect_is(whos_ready_2, 'logical')
  expect_length(whos_ready_2, 3)
  expect_true(whos_ready_2['foo'])
  expect_true(is.na(whos_ready_2['bar']))
  expect_true(is.na(whos_ready_2['baz']))
})

################################################################################
# cleaning
unlink('received_data', recursive = TRUE)
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)
unlink(c('sfn_monitor.Rmd','main_script.R','debug_script.R'))



