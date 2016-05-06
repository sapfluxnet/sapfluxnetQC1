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
          to = c('Received_data/foo_sapflow.csv',
                 'Received_data/foo_metadata.xlsx',
                 'Received_data/foo_environmental.csv'))

df_received_to_accepted(remove = FALSE)

received_md5 <- md5sum(c('Received_data/foo_sapflow.csv',
                         'Received_data/foo_metadata.xlsx',
                         'Received_data/foo_environmental.csv'))
accepted_md5 <- md5sum(c('Data/foo/Accepted/foo_sapflow.csv',
                          'Data/foo/Accepted/foo_metadata.xlsx',
                          'Data/foo/Accepted/foo_environmental.csv'))

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
                 'Removing the received files...')
})

unlink('Received_data', recursive = TRUE)
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)
