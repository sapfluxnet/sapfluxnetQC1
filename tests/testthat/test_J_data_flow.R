library(sapfluxnetr)
library(tools)

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

context('J2. received_to_accepted')

dir.create('Received_data')
file.copy(from = c('foo.csv', 'foo.xlsx', 'foo_env.csv'),
          to = c('Received_data/foo_sapflow_data.csv',
                 'Received_data/foo_metadata.xlsx',
                 'Received_data/foo_env_data.csv'))

suppressMessages(df_received_to_accepted(remove = FALSE))

received_md5 <- md5sum(c('Received_data/foo_sapflow_data.csv',
                         'Received_data/foo_metadata.xlsx',
                         'Received_data/foo_env_data.csv'))
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
  expect_false(file.exists('Received_data/foo_sapflow_data.csv'))
  expect_false(file.exists('Received_data/foo_env_data.csv'))
  expect_false(file.exists('Received_data/foo_metadata.xlsx'))
})

file.copy(from = c('foo.csv', 'foo.xlsx', 'foo_env.csv'),
          to = c('Received_data/bar_sapflow.csv',
                 'Received_data/bar.xlsx',
                 'Received_data/bar_environmental.csv'))

suppressMessages(df_received_to_accepted(remove = FALSE))

test_that('bar site files are not copied', {
  expect_false(dir.exists('Data/bar'))
  expect_false(file.exists('Data/bar/Accepted/bar.xlsx'))
  expect_false(file.exists('Data/bar/Accepted/bar_sapflow.csv'))
  expect_false(file.exists('Data/bar/Accepted/bar_environmental.csv'))
})

unlink('Received_data', recursive = TRUE)
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)


context('J3. Status yaml files')

df_folder_structure()

dir.create('Received_data')

file.copy(from = c('foo.csv', 'foo.xlsx', 'foo_env.csv'),
          to = c('Received_data/foo_sapflow_data.csv',
                 'Received_data/foo_metadata.xlsx',
                 'Received_data/foo_env_data.csv'))

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

unlink('Received_data', recursive = TRUE)
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)
