library(sapfluxnetr)

################################################################################
context('M1. Get the code and the data files route')

dir.create('FakeData')

test_that('Argument checks work', {
  expect_error(dl_get_si_code('WrongDir'), 'Folder does not exist')
})

test_that('Stop if no files are found', {
  expect_error(dl_get_si_code('FakeData'),
               'There is no files matching data names pattern')
})

file.copy(from = c('foo.xlsx', 'foo.xlsx'),
          to = c('FakeData/foo_metadata.xlsx',
                 'FakeData/bar_metadata.xlsx'))

test_that('Stop when more than one code in the folder', {
  expect_error(dl_get_si_code('FakeData'),
               'There is more than one code in the folder')
})

unlink('FakeData/bar_metadata.xlsx')

res_one <- dl_get_si_code('FakeData')

file.copy(from = c('foo.csv', 'foo_env.csv'),
          to = c('FakeData/foo_sapflow_data.csv',
                 'FakeData/foo_env_data.csv'))

res_three <- dl_get_si_code('FakeData')

ex_one <- list(si_code = 'foo',
               md_file = 'FakeData/foo_metadata.xlsx',
               sapf_file = 'FakeData/foo_metadata.xlsx',
               env_file = 'FakeData/foo_metadata.xlsx')

ex_three <- list(si_code = 'foo',
               md_file = 'FakeData/foo_metadata.xlsx',
               sapf_file = 'FakeData/foo_sapflow_data.csv',
               env_file = 'FakeData/foo_env_data.csv')

test_that('results are correct', {
  expect_is(res_one, 'list')
  expect_is(res_three, 'list')
  expect_length(res_one, 4)
  expect_length(res_three, 4)
  expect_identical(res_one, ex_one)
  expect_identical(res_three, ex_three)
})

################################################################################
context('M2. QC Start Process')

test_that('Argument checks work', {
  expect_error(qc_start_process(25),
               'folder provided is not a character string')
  expect_error(qc_start_process(data.frame(a = 'lalala', b = 'lololo')),
               'folder provided is not a character string')
})

# preparation
dir.create('received_data')
df_folder_structure()

file.copy(
  system.file("Rmd_templates", "received_to_accepted.Rmd", package = "sapfluxnetr"),
  file.path('Templates', "received_to_accepted.Rmd")
)
file.copy(
  system.file("Rmd_templates", "QC_report.Rmd", package = "sapfluxnetr"),
  file.path('Templates', "QC_report.Rmd")
)

file.copy(
  c('foo.xlsx', 'foo.xlsx'),
  c(file.path('received_data', 'foo_metadata.xlsx'),
    file.path('received_data', 'bar_metadata.xlsx'))
)

rep_sfn_render('received_to_accepted.Rmd',
               output_file = file.path(
                 'Reports', paste(format(Sys.time(), '%Y%m%d%H%M'),
                                  'received_to_accepted.html', sep = '_')
               ),
               output_dir = 'Reports')

data_folders <- df_get_data_folders()

# I can't test the rdata file as in test environment objects are not found :(

for (folder in data_folders) {
  suppressWarnings(
    suppressMessages(qc_start_process(file.path(folder, 'Accepted'),
                                      rdata = FALSE))
  )
}

foo_yaml <- df_get_status('foo')
bar_yaml <- df_get_status('bar')

test_that('files are created OK and in the correct places', {
  expect_true(file.exists(file.path('Data', 'foo', 'Accepted', 'foo_metadata.xlsx')))
  expect_true(file.exists(file.path('Data', 'bar', 'Accepted', 'bar_metadata.xlsx')))
  expect_true(file.exists(file.path('Logs', 'sapfluxnet.log')))
  expect_true(file.exists(file.path('Templates', 'received_to_accepted.Rmd')))
  expect_true(file.exists(file.path('Templates', 'QC_report.Rmd')))
  expect_true(file.exists(file.path('Data', 'foo', 'foo_status.yaml')))
  expect_true(file.exists(file.path('Data', 'bar', 'bar_status.yaml')))

  expect_length(list.files(file.path('Reports', 'foo'),
                           pattern = 'QC_report.html'), 1)
  expect_length(list.files(file.path('Reports', 'bar'),
                           pattern = 'QC_report.html'), 1)
  expect_length(list.files(file.path('Reports'),
                           pattern = 'received_to_accepted.html'), 1)



  expect_is(foo_yaml, 'list')
  expect_true(foo_yaml$QC$DONE)
  expect_true(foo_yaml$LVL1$STORED)
  expect_false(foo_yaml$LVL2$STORED)
  expect_true(!is.null(foo_yaml$QC$DATE))
  expect_true(!is.null(foo_yaml$LVL1$DATE))
  expect_true(is.null(foo_yaml$LVL2$DATE))
  expect_is(bar_yaml, 'list')
  expect_true(bar_yaml$QC$DONE)
  expect_true(bar_yaml$LVL1$STORED)
  expect_false(bar_yaml$LVL2$STORED)
  expect_true(!is.null(bar_yaml$QC$DATE))
  expect_true(!is.null(bar_yaml$LVL1$DATE))
  expect_true(is.null(bar_yaml$LVL2$DATE))
})


################################################################################
# cleaning
unlink('FakeData', recursive = TRUE)
unlink('received_data', recursive = TRUE)
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)
unlink('ESP_adm0.rds')
