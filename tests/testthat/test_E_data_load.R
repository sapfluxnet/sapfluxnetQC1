library(sapfluxnetr)

context('E1. Data load')

foo_object <- 25

# foo file
zz <- file("foo.data", "w")  # open an output file connection
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
cat("One more line\n", file = zz)
close(zz)

test_that('Error raises in case of bad arguments', {
  expect_error(dl_data('file_that_not_exists', 'site_md'),
               'File does not exist')
  expect_error(dl_data(foo_object, 'site_md'),
               'File name is not provided as character')
  expect_error(dl_data('foo.data', 'not_a_metadata_sheet'),
               'Provided sheet name is not a character or is not a metadata')
  expect_error(dl_data('foo.data', foo_object),
               'Provided sheet name is not a character or is not a metadata')
})

file.remove('foo.data')

file_name <- 'foo.xlsx'
sapflow_data <- dl_data(file_name, 'sapflow_hd', long = FALSE)
sapflow_data_long <- dl_data(file_name, 'sapflow_hd', long = TRUE)
env_data <- dl_data(file_name, 'environmental_hd', long = FALSE)
env_data_long <- dl_data(file_name, 'environmental_hd', long = TRUE)

test_that('function returns data frames', {
  expect_is(sapflow_data, 'data.frame')
  expect_is(sapflow_data_long, 'data.frame')
  expect_is(env_data, 'data.frame')
  expect_is(env_data_long, 'data.frame')
})
