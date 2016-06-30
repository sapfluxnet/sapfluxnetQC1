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
# cleaning
unlink('FakeData', recursive = TRUE)
