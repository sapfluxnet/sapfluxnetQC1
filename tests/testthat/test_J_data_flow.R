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
  expect_equal(foo_yaml$LVL1$TO_LVL2, 'FREEZE')
  expect_null(foo_yaml$QC$DATE)
  expect_null(foo_yaml$LVL1$DATE)
  expect_null(foo_yaml$LVL2$DATE)
  expect_null(foo_yaml$LVL2$STEP)
  expect_equal(foo_yaml$LVL2$TO_REM, 'FREEZE')
  expect_equal(foo_yaml$LVL2$TO_UNITS, 'FREEZE')
  expect_null(foo_yaml$LVL2$AVAIL)

  expect_true(df_set_status('foo', QC = list(DONE = TRUE,
                                             DATE = as.character(Sys.Date())),
                            LVL2 = list(AVAIL = c('plant', 'sapwood'))))

  foo_yaml <- df_get_status('foo')

  expect_true(foo_yaml$QC$DONE)
  expect_is(foo_yaml$QC$DATE, 'character')
  expect_false(foo_yaml$LVL1$STORED)
  expect_false(foo_yaml$LVL2$STORED)
  expect_equal(foo_yaml$LVL1$TO_LVL2, 'FREEZE')
  expect_null(foo_yaml$LVL1$DATE)
  expect_null(foo_yaml$LVL2$DATE)
  expect_null(foo_yaml$LVL2$STEP)
  expect_equal(foo_yaml$LVL2$TO_REM, 'FREEZE')
  expect_equal(foo_yaml$LVL2$TO_UNITS, 'FREEZE')
  expect_is(foo_yaml$LVL2$AVAIL, 'character')
  expect_length(foo_yaml$LVL2$AVAIL, 2)

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

file.create('Data/legen_wait_for_it/Accepted/dary.xlsx')
old_yaml <- df_get_status(code)
old_files_lvl1 <- c(list.files(file.path('Data', code, 'Lvl_1'), full.names = TRUE))
old_files_acc <- c(list.files(file.path('Data', code, 'Accepted'), full.names = TRUE))

df_reset_data_status(code, level = 'LVL1')

new_yaml <- df_get_status(code)
new_files_lvl1 <- c(list.files(file.path('Data', code, 'Lvl_1'), full.names = TRUE))
new_files_acc <- c(list.files(file.path('Data', code, 'Accepted'), full.names = TRUE))

test_that('status file has been updated', {
  expect_true(old_yaml$QC$DONE)
  expect_true(new_yaml$QC$DONE)
  expect_equal(old_yaml$QC$DATE, as.character(Sys.Date()))
  expect_equal(new_yaml$QC$DATE, as.character(Sys.Date()))
  expect_true(old_yaml$LVL1$STORED)
  expect_false(new_yaml$LVL1$STORED)
  expect_equal(old_yaml$LVL1$DATE, as.character(Sys.Date()))
  expect_null(new_yaml$LVL1$DATE)
})

test_that('files had been renamed correctly', {
  expect_false(all(old_files_lvl1 == new_files_lvl1))
  expect_true(length(old_files_lvl1) == length(new_files_lvl1))
  expect_match(new_files_lvl1, "(.bak)$")
  expect_match(old_files_lvl1, "(.csv|.RData|.xlsx)$")
  expect_true(all(old_files_acc == new_files_acc))
  expect_match(new_files_acc, "(.csv|.RData|.xlsx)$")
  expect_match(old_files_acc, "(.csv|.RData|.xlsx)$")
})

df_reset_data_status(code, level = 'all')

new_yaml <- df_get_status(code)
new_files_lvl1 <- c(list.files(file.path('Data', code, 'Lvl_1'), full.names = TRUE))
new_files_acc <- c(list.files(file.path('Data', code, 'Accepted'), full.names = TRUE))

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
  expect_false(all(old_files_lvl1 == new_files_lvl1))
  expect_true(length(old_files_lvl1) == length(new_files_lvl1))
  expect_match(new_files_lvl1, "(.bak)$")
  expect_match(old_files_lvl1, "(.csv|.RData|.xlsx)$")
  expect_false(all(old_files_acc == new_files_acc))
  expect_match(new_files_acc, "(.bak)$")
  expect_match(old_files_acc, "(.csv|.RData|.xlsx)$")
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
context('J9. who is ready to any level?')

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
  LVL1 = list(STORED = TRUE, DATE = Sys.Date(), TO_LVL2 = 'READY')
)

df_set_status(
  'bar',
  QC = list(DONE = TRUE, DATE = Sys.Date()),
  LVL1 = list(STORED = TRUE, DATE = Sys.Date(), TO_LVL2 = 'FREEZE')
)

df_set_status(
  'baz',
  QC = list(DONE = TRUE, DATE = Sys.Date()),
  LVL1 = list(STORED = TRUE, DATE = Sys.Date(), TO_LVL2 = 'DONE'),
  LVL2 = list(STORED = TRUE, DATE = Sys.Date(), STEP = 'UNITS',
              TO_REM = 'DONE', TO_UNITS = 'DONE')
)

# testing if function works
whos_ready_lvl2 <- df_whos_ready_to(level = 'lvl2', filter = 'all')
whos_ready_rem <- df_whos_ready_to(level = 'rem', filter = 'all')
whos_ready_units <- df_whos_ready_to(level = 'units', filter = 'all')

test_that('results are as expected',{
  expect_is(whos_ready_lvl2, 'list')
  expect_is(whos_ready_rem, 'list')
  expect_is(whos_ready_units, 'list')
  expect_length(whos_ready_lvl2, 3)
  expect_length(whos_ready_rem, 3)
  expect_length(whos_ready_units, 3)
  expect_identical(whos_ready_lvl2[['foo']], 'READY')
  expect_identical(whos_ready_lvl2[['bar']], 'FREEZE' )
  expect_identical(whos_ready_lvl2[['baz']], 'DONE')
  expect_identical(whos_ready_rem[['foo']], 'FREEZE')
  expect_identical(whos_ready_rem[['bar']], 'FREEZE' )
  expect_identical(whos_ready_rem[['baz']], 'DONE')
  expect_identical(whos_ready_units[['foo']], 'FREEZE')
  expect_identical(whos_ready_units[['bar']], 'FREEZE' )
  expect_identical(whos_ready_units[['baz']], 'DONE')
})

# testing filter argument
whos_ready_true_lvl2 <- df_whos_ready_to('lvl2', filter = 'ready')
whos_ready_freeze_lvl2 <- df_whos_ready_to('lvl2', filter = 'freeze')
whos_ready_done_lvl2 <- df_whos_ready_to('lvl2', filter = 'done')

whos_ready_true_rem <- df_whos_ready_to('rem', filter = 'ready')
whos_ready_freeze_rem <- df_whos_ready_to('rem', filter = 'freeze')
whos_ready_done_rem <- df_whos_ready_to('rem', filter = 'done')

whos_ready_true_units <- df_whos_ready_to('units', filter = 'ready')
whos_ready_freeze_units <- df_whos_ready_to('units', filter = 'freeze')
whos_ready_done_units <- df_whos_ready_to('units', filter = 'done')

test_that('filtering is as expected',{
  expect_is(whos_ready_true_lvl2, 'list')
  expect_length(whos_ready_true_lvl2, 1)
  expect_identical(whos_ready_true_lvl2[['foo']], 'READY')
  expect_is(whos_ready_freeze_lvl2, 'list')
  expect_length(whos_ready_freeze_lvl2, 1)
  expect_identical(whos_ready_freeze_lvl2[['bar']], 'FREEZE' )
  expect_is(whos_ready_done_lvl2, 'list')
  expect_length(whos_ready_done_lvl2, 1)
  expect_identical(whos_ready_done_lvl2[['baz']], 'DONE')

  expect_is(whos_ready_true_rem, 'list')
  expect_length(whos_ready_true_rem, 0)
  expect_is(whos_ready_freeze_rem, 'list')
  expect_length(whos_ready_freeze_rem, 2)
  expect_identical(whos_ready_freeze_rem[['foo']], 'FREEZE' )
  expect_identical(whos_ready_freeze_rem[['bar']], 'FREEZE' )
  expect_is(whos_ready_done_rem, 'list')
  expect_length(whos_ready_done_rem, 1)
  expect_identical(whos_ready_done_rem[['baz']], 'DONE')

  expect_is(whos_ready_true_units, 'list')
  expect_length(whos_ready_true_units, 0)
  expect_is(whos_ready_freeze_units, 'list')
  expect_length(whos_ready_freeze_units, 2)
  expect_identical(whos_ready_freeze_units[['foo']], 'FREEZE' )
  expect_identical(whos_ready_freeze_units[['bar']], 'FREEZE' )
  expect_is(whos_ready_done_units, 'list')
  expect_length(whos_ready_done_units, 1)
  expect_identical(whos_ready_done_units[['baz']], 'DONE')
})


# testing if empty
unlink(file.path('Data', 'bar', 'bar_status.yaml'))

whos_ready_2 <- suppressWarnings(df_whos_ready_to('lvl2', filter = 'all'))

test_that('if not status NA is returned', {
  expect_is(whos_ready_2, 'list')
  expect_length(whos_ready_2, 3)
  expect_identical(whos_ready_2[['foo']], 'READY')
  expect_true(is.na(whos_ready_2['bar']))
  expect_identical(whos_ready_2[['baz']], 'DONE')
})

################################################################################
context('J10. lvl2_folder_structure')

dir.create(file.path('Data', 'foo', 'Lvl_2'))

df_lvl2_folder_structure('foo')

test_that('folders are created ok', {
  expect_true(dir.exists(file.path('Data', 'foo',
                                   'Lvl_2', 'lvl_2_out_warn')))
  expect_true(dir.exists(file.path('Data', 'foo',
                                   'Lvl_2', 'lvl_2_out_rem')))
  expect_true(dir.exists(file.path('Data', 'foo',
                                   'Lvl_2', 'lvl_2_unit_trans')))
  expect_true(dir.exists(file.path('Data', 'foo',
                                   'Lvl_2', 'lvl_2_unit_trans',
                                   'plant')))
  expect_true(dir.exists(file.path('Data', 'foo',
                                   'Lvl_2', 'lvl_2_unit_trans',
                                   'sapwood')))
  expect_true(dir.exists(file.path('Data', 'foo',
                                   'Lvl_2', 'lvl_2_unit_trans',
                                   'leaf')))
})

test_that('function throws an error when not creating the folders', {
  expect_error(suppressWarnings(df_lvl2_folder_structure('xyz')),
               'One or more folders can not be created in level 2')
})

################################################################################
context('J11. loading SfnData object')

dir.create(file.path('Data', 'foo', 'Lvl_1'))
dir.create(file.path('Data', 'bar', 'Lvl_1'))
dir.create(file.path('Data', 'bar', 'Lvl_2'))
dir.create(file.path('Data', 'baz', 'Lvl_1'))
dir.create(file.path('Data', 'baz', 'Lvl_2'))

df_lvl2_folder_structure('bar')
df_lvl2_folder_structure('baz')

load('FOO.RData')
foo <- FOO
get_si_code(foo) <- rep('foo', length(get_si_code(foo)))
bar <- FOO
get_si_code(bar) <- rep('bar', length(get_si_code(bar)))

save(foo, file = file.path('Data', 'foo', 'Lvl_1', 'foo.RData'))
save(foo, file = file.path('Data', 'foo', 'Lvl_2', 'foo.RData'))
save(foo, file = file.path('Data', 'foo', 'Lvl_2', 'lvl_2_out_warn', 'foo.RData'))
save(foo, file = file.path('Data', 'foo', 'Lvl_2', 'lvl_2_out_rem', 'foo.RData'))
# save(foo, file = file.path('Data', 'foo', 'Lvl_2', 'lvl_2_unit_trans', 'foo.RData'))

save(bar, file = file.path('Data', 'bar', 'Lvl_1', 'bar.RData'))
save(bar, file = file.path('Data', 'bar', 'Lvl_2', 'bar.RData'))
save(bar, file = file.path('Data', 'bar', 'Lvl_2', 'lvl_2_out_warn', 'bar.RData'))
save(bar, file = file.path('Data', 'bar', 'Lvl_2', 'lvl_2_out_rem', 'bar.RData'))
# save(bar, file = file.path('Data', 'bar', 'Lvl_2', 'lvl_2_unit_trans', 'bar.RData'))
save(bar, file = file.path('Data', 'bar', 'Lvl_2',
                           'lvl_2_unit_trans', 'plant', 'bar.RData'))
save(bar, file = file.path('Data', 'bar', 'Lvl_2',
                           'lvl_2_unit_trans', 'sapwood', 'bar.RData'))

test_that('objects are loaded fine or rise an error', {
  expect_is(df_read_SfnData('foo', 'Lvl_1'), 'SfnData')
  expect_is(df_read_SfnData('foo', 'Lvl_2'), 'SfnData')
  expect_is(df_read_SfnData('foo', 'out_warn'), 'SfnData')
  expect_is(df_read_SfnData('foo', 'out_rem'), 'SfnData')
  # expect_is(df_read_SfnData('foo', 'unit_trans'), 'SfnData')
  expect_is(df_read_SfnData('bar', 'Lvl_1'), 'SfnData')
  expect_is(df_read_SfnData('bar', 'Lvl_2'), 'SfnData')
  expect_is(df_read_SfnData('bar', 'out_warn'), 'SfnData')
  expect_is(df_read_SfnData('bar', 'out_rem'), 'SfnData')
  # expect_is(df_read_SfnData('bar', 'unit_trans'), 'SfnData')
  expect_is(df_read_SfnData('bar', 'unit_trans', 'plant'), 'SfnData')
  expect_is(df_read_SfnData('bar', 'unit_trans', 'sapwood'), 'SfnData')
  expect_error(df_read_SfnData('baz', 'Lvl_1'),
               'does not exist.')
  expect_error(df_read_SfnData('baz', 'Lvl_2'),
               'does not exist.')
  expect_error(df_read_SfnData('baz', 'out_warn'),
               'does not exist.')
  expect_error(df_read_SfnData('baz', 'out_rem'),
               'does not exist.')
  # expect_error(df_read_SfnData('baz', 'unit_trans'),
  #              'does not exist.')
  expect_error(df_read_SfnData('bar', 'unit_trans', 'leaf'),
               'does not exist')
})

################################################################################
context('J12. Write SfnData to location')

baz <- FOO
get_si_code(baz) <- rep('baz', length(get_si_code(baz)))
df_write_SfnData(baz, 'Lvl_1')
df_write_SfnData(baz, 'Lvl_2')
df_write_SfnData(baz, 'out_warn')
df_write_SfnData(baz, 'out_rem')
# df_write_SfnData(baz, 'unit_trans')
df_write_SfnData(baz, 'unit_trans', 'leaf')

test_that('objects are written to RData files fine', {
  expect_true(file.exists(file.path('Data', 'baz', 'Lvl_1', 'baz.RData')))
  expect_true(file.exists(file.path('Data', 'baz', 'Lvl_2', 'baz.RData')))
  expect_true(file.exists(file.path('Data', 'baz', 'Lvl_2',
                                    'lvl_2_out_warn', 'baz.RData')))
  expect_true(file.exists(file.path('Data', 'baz', 'Lvl_2',
                                    'lvl_2_out_rem', 'baz.RData')))
  # expect_true(file.exists(file.path('Data', 'baz', 'Lvl_2',
  #                                   'lvl_2_unit_trans', 'baz.RData')))
  expect_true(file.exists(file.path('Data', 'baz', 'Lvl_2',
                                    'lvl_2_unit_trans', 'leaf', 'baz.RData')))
  expect_equal(df_get_status('baz')$LVL1$TO_LVL2, 'DONE')
  expect_equal(df_get_status('baz')$LVL2$AVAIL, c('leaf'))
})

test_that('error rise if file already exists', {
  expect_error(df_write_SfnData(baz, 'Lvl_1'),
               'object already exists')
  expect_error(df_write_SfnData(baz, 'Lvl_2'),
               'object already exists')
  expect_error(df_write_SfnData(baz, 'out_warn'),
               'object already exists')
  expect_error(df_write_SfnData(baz, 'out_rem'),
               'object already exists')
  # expect_error(df_write_SfnData(baz, 'unit_trans'),
  #              'object already exists')
  expect_error(df_write_SfnData(baz, 'unit_trans', 'leaf'),
               'object already exists')
  expect_error(df_write_SfnData(bar, 'Lvl_1'),
               'object already exists')
  expect_error(df_write_SfnData(bar, 'Lvl_2'),
               'object already exists')
  expect_error(df_write_SfnData(bar, 'out_warn'),
               'object already exists')
  expect_error(df_write_SfnData(bar, 'out_rem'),
               'object already exists')
  # expect_error(df_write_SfnData(bar, 'unit_trans'),
  #              'object already exists')
  expect_error(df_write_SfnData(foo, 'Lvl_1'),
               'object already exists')
  expect_error(df_write_SfnData(foo, 'Lvl_2'),
               'object already exists')
  expect_error(df_write_SfnData(foo, 'out_warn'),
               'object already exists')
  expect_error(df_write_SfnData(foo, 'out_rem'),
               'object already exists')
  # expect_error(df_write_SfnData(foo, 'unit_trans'),
  #              'object already exists')
})

################################################################################
context('J7b. Reset data and status (level 2)')

code <- 'foo'

dir.create(file.path('Data', 'foo', 'Accepted'))
file.create('Data/foo/Accepted/foo.xlsx')
file.create('Data/foo/Lvl_2/lvl_2_out_warn/foo_out_to_remove.txt')
df_write_SfnData(foo, 'unit_trans', 'plant')

df_set_status(
  'foo',
  QC = list(STORED = TRUE, DATE = as.character(Sys.Date())),
  LVL1 = list(STORED = TRUE, DATE = as.character(Sys.Date()), TO_LVL2 = 'DONE'),
  LVL2 = list(STORED = TRUE, DATE = as.character(Sys.Date()),
              TO_REM = 'DONE', TO_UNITS = 'DONE', AVAIL = c('plant'))
)

old_yaml <- df_get_status(code)
old_files_lvl2 <- c(list.files(file.path('Data', code, 'Lvl_2'),
                               full.names = TRUE, recursive = TRUE))
old_files_lvl1 <- c(list.files(file.path('Data', code, 'Lvl_1'), full.names = TRUE))
old_files_acc <- c(list.files(file.path('Data', code, 'Accepted'), full.names = TRUE))

df_reset_data_status(code, level = 'LVL2')

new_yaml <- df_get_status(code)
new_files_lvl2 <- c(list.files(file.path('Data', code, 'Lvl_2'),
                               full.names = TRUE, recursive = TRUE))
new_files_lvl1 <- c(list.files(file.path('Data', code, 'Lvl_1'), full.names = TRUE))
new_files_acc <- c(list.files(file.path('Data', code, 'Accepted'), full.names = TRUE))

test_that('status file has been updated', {
  expect_true(old_yaml$QC$DONE)
  expect_true(new_yaml$QC$DONE)
  expect_equal(old_yaml$QC$DATE, as.character(Sys.Date()))
  expect_equal(new_yaml$QC$DATE, as.character(Sys.Date()))
  expect_true(old_yaml$LVL1$STORED)
  expect_true(new_yaml$LVL1$STORED)
  expect_equal(old_yaml$LVL1$DATE, as.character(Sys.Date()))
  expect_equal(new_yaml$LVL1$DATE, as.character(Sys.Date()))
  expect_equal(old_yaml$LVL1$TO_LVL2, 'DONE')
  expect_equal(new_yaml$LVL1$TO_LVL2, 'FREEZE')
  expect_true(old_yaml$LVL2$STORED)
  expect_false(new_yaml$LVL2$STORED)
  expect_equal(old_yaml$LVL2$DATE, as.character(Sys.Date()))
  expect_null(new_yaml$LVL2$DATE)
  expect_equal(old_yaml$LVL2$TO_REM, 'DONE')
  expect_equal(old_yaml$LVL2$TO_UNITS, 'DONE')
  expect_equal(new_yaml$LVL2$TO_REM, 'FREEZE')
  expect_equal(new_yaml$LVL2$TO_UNITS, 'FREEZE')
  expect_null(new_yaml$LVL2$AVAIL)
})

test_that('files had been renamed correctly', {
  expect_true(all(old_files_lvl1 == new_files_lvl1))
  expect_true(length(old_files_lvl1) == length(new_files_lvl1))
  expect_match(new_files_lvl1, "(.csv|.RData|.xlsx)$")
  expect_match(old_files_lvl1, "(.csv|.RData|.xlsx)$")
  expect_true(all(old_files_acc == new_files_acc))
  expect_match(new_files_acc, "(.csv|.RData|.xlsx)$")
  expect_match(old_files_acc, "(.csv|.RData|.xlsx)$")
  expect_false(all(old_files_lvl2 == new_files_lvl2))
  expect_true(length(old_files_lvl2) == length(new_files_lvl2))
  expect_match(new_files_lvl2, "(.bak)$")
  expect_match(old_files_lvl2, "(.csv|.RData|.xlsx|.txt)$")
})

################################################################################
context('J13. df_lvl1_to_lvl2 data flow')

unlink(file.path('Data', 'foo', 'Lvl_2'), recursive = TRUE)
unlink(file.path('Data', 'bar', 'Lvl_2'), recursive = TRUE)
unlink(file.path('Data', 'baz', 'Lvl_2'), recursive = TRUE)

save(baz, file = file.path('Data', 'baz', 'Lvl_1', 'baz.RData'))
df_start_status('bar')

dir.create(file.path('Data', 'foo', 'Lvl_2'))
dir.create(file.path('Data', 'bar', 'Lvl_2'))
dir.create(file.path('Data', 'baz', 'Lvl_2'))

df_reset_data_status('baz', 'LVL2') # because what we did in J9

df_set_status('foo', LVL1 = list(TO_LVL2 = 'READY'))
df_set_status('bar', LVL1 = list(TO_LVL2 = 'READY'))
df_set_status('baz', LVL1 = list(TO_LVL2 = 'FREEZE'))

df_lvl1_to_lvl2()

test_that('files are moved fine', {
  expect_true(file.exists(file.path('Data', 'foo', 'Lvl_2', 'lvl_2_out_warn', 'foo.RData')))
  expect_true(file.exists(file.path('Data', 'bar', 'Lvl_2', 'lvl_2_out_warn', 'bar.RData')))
  expect_false(file.exists(file.path('Data', 'baz', 'Lvl_2', 'lvl_2_out_warn', 'baz.RData')))
  expect_equal(df_get_status('foo')$LVL1$TO_LVL2, 'DONE')
  expect_equal(df_get_status('bar')$LVL1$TO_LVL2, 'DONE')
  expect_equal(df_get_status('baz')$LVL1$TO_LVL2, 'FREEZE')
})

test_that('status slot LVL2$STEP is updated', {
  expect_equal(df_get_status('foo')$LVL2$STEP, 'WARN')
  expect_equal(df_get_status('bar')$LVL2$STEP, 'WARN')
  expect_null(df_get_status('baz')$LVL2$STEP)
})

df_set_status('baz', LVL1 = list(TO_LVL2 = 'READY'))
df_lvl1_to_lvl2()

test_that('DONE sites are respected', {
  expect_true(file.exists(file.path('Data', 'foo', 'Lvl_2', 'lvl_2_out_warn', 'foo.RData')))
  expect_true(file.exists(file.path('Data', 'bar', 'Lvl_2', 'lvl_2_out_warn', 'bar.RData')))
  expect_true(file.exists(file.path('Data', 'baz', 'Lvl_2', 'lvl_2_out_warn', 'baz.RData')))
  expect_equal(df_get_status('foo')$LVL1$TO_LVL2, 'DONE')
  expect_equal(df_get_status('bar')$LVL1$TO_LVL2, 'DONE')
  expect_equal(df_get_status('baz')$LVL1$TO_LVL2, 'DONE')
})



################################################################################
# cleaning
unlink('received_data', recursive = TRUE)
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)
unlink(c('sfn_monitor.Rmd','main_script.R','debug_script.R'))
