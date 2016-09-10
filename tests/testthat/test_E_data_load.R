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

xlsx_name <- 'foo.xlsx'
csv_name <- 'foo.csv'
csv_name_2 <- 'foo_env.csv'

sapflow_data <- suppressMessages(dl_data(xlsx_name, 'sapflow_hd',
                                         long = FALSE))
# sapflow_data_long <- suppressMessages(dl_data(xlsx_name,
#                                               'sapflow_hd',
#                                               long = TRUE))
env_data <- suppressMessages(dl_data(xlsx_name, 'environmental_hd', long = FALSE))
# env_data_long <- suppressMessages(dl_data(xlsx_name, 'environmental_hd',
#                                           long = TRUE))

sapf_csv_data <- suppressMessages(dl_data(csv_name, 'sapflow_hd',
                                          long = FALSE, n = 50000))
# sapf_csv_data_long <- suppressMessages(dl_data(csv_name, 'sapflow_hd',
#                                                long = TRUE, n = 50000))
env_csv_data <- suppressMessages(dl_data(csv_name_2, 'environmental_hd',
                                         long = FALSE, n = 50000))
# env_csv_data_long <- suppressMessages(dl_data(csv_name_2, 'environmental_hd',
#                                               long = TRUE, n = 50000))

xlsx_name <- 'foo_nan.xlsx'
csv_name <- 'foo_nan.csv'
csv_name_2 <- 'foo_nan_env.csv'

sapflow_data_nan <- suppressMessages(dl_data(xlsx_name, 'sapflow_hd',
                                             na = 'NaN', long = FALSE))
# sapflow_data_long_nan <- suppressMessages(dl_data(xlsx_name, 'sapflow_hd',
#                                                   na = 'NaN', long = TRUE))
env_data_nan <- suppressMessages(dl_data(xlsx_name, 'environmental_hd',
                                         na = 'NaN', long = FALSE))
# env_data_long_nan <- suppressMessages(dl_data(xlsx_name, 'environmental_hd',
#                                               na = 'NaN', long = TRUE))

sapf_csv_data_nan <- suppressMessages(dl_data(csv_name, 'sapflow_hd',
                                              long = FALSE, na = 'NaN', n = 50000))
# sapf_csv_data_long_nan <- suppressMessages(dl_data(csv_name, 'sapflow_hd',
#                                                    long = TRUE, na = 'NaN', n = 50000))
env_csv_data_nan <- suppressMessages(dl_data(csv_name_2, 'environmental_hd',
                                             long = FALSE, na = 'NaN', n = 50000))
# env_csv_data_long_nan <- suppressMessages(dl_data(csv_name_2, 'environmental_hd',
#                                                   long = TRUE, na = 'NaN', n = 50000))


test_that('function returns data frames', {
  expect_is(sapflow_data, 'data.frame')
  # expect_is(sapflow_data_long, 'data.frame')
  expect_is(env_data, 'data.frame')
  # expect_is(env_data_long, 'data.frame')
  expect_is(sapf_csv_data, 'data.frame')
  # expect_is(sapf_csv_data_long, 'data.frame')
  expect_is(env_csv_data, 'data.frame')
  # expect_is(env_csv_data_long, 'data.frame')
  expect_is(sapflow_data_nan, 'data.frame')
  # expect_is(sapflow_data_long_nan, 'data.frame')
  expect_is(env_data_nan, 'data.frame')
  # expect_is(env_data_long_nan, 'data.frame')
  expect_is(sapf_csv_data_nan, 'data.frame')
  # expect_is(sapf_csv_data_long_nan, 'data.frame')
  expect_is(env_csv_data_nan, 'data.frame')
  # expect_is(env_csv_data_long_nan, 'data.frame')
})

test_that('number of variables is correct', {
  expect_equal(length(names(sapflow_data)), 14)
  # expect_equal(length(names(sapflow_data_long)), 3)
  expect_equal(length(names(env_data)), 7)
  # expect_equal(length(names(env_data_long)), 3)
  expect_equal(length(names(sapflow_data)), 14)
  # expect_equal(length(names(sapflow_data_long)), 3)
  expect_equal(length(names(env_data)), 7)
  # expect_equal(length(names(env_data_long)), 3)
  expect_equal(length(names(sapflow_data_nan)), 14)
  # expect_equal(length(names(sapflow_data_long_nan)), 3)
  expect_equal(length(names(env_data_nan)), 7)
  # expect_equal(length(names(env_data_long_nan)), 3)
  expect_equal(length(names(sapflow_data_nan)), 14)
  # expect_equal(length(names(sapflow_data_long_nan)), 3)
  expect_equal(length(names(env_data_nan)), 7)
  # expect_equal(length(names(env_data_long_nan)), 3)
})

test_that('number of rows is as expected', {
  expect_equal(nrow(sapflow_data), 78816)
  # expect_equal(nrow(sapflow_data_long), 1024608)
  expect_equal(nrow(env_data), 78816)
  # expect_equal(nrow(env_data_long), 472896)
  expect_equal(nrow(sapflow_data), 78816)
  # expect_equal(nrow(sapflow_data_long), 1024608)
  expect_equal(nrow(env_data), 78816)
  # expect_equal(nrow(env_data_long), 472896)
  expect_equal(nrow(sapflow_data_nan), 78816)
  # expect_equal(nrow(sapflow_data_long_nan), 1024608)
  expect_equal(nrow(env_data_nan), 78816)
  # expect_equal(nrow(env_data_long_nan), 472896)
  expect_equal(nrow(sapflow_data_nan), 78816)
  # expect_equal(nrow(sapflow_data_long_nan), 1024608)
  expect_equal(nrow(env_data_nan), 78816)
  # expect_equal(nrow(env_data_long_nan), 472896)
})

test_that('NaN and "Missing" are interpreted as they must', {
  expect_is(sapflow_data[[2]], 'numeric')
  expect_equal(sum(!is.na(sapflow_data_nan[[2]])), 0)
  expect_is(sapflow_data[[7]], 'numeric')
  expect_is(sapflow_data_nan[[7]], 'numeric')
  expect_is(env_data[['ta']], 'numeric')
  expect_equal(sum(!is.na(env_data_nan[['ta']])), 0)
  expect_is(sapf_csv_data[[2]], 'numeric')
  expect_is(sapf_csv_data_nan[[2]], 'logical')
  expect_is(sapf_csv_data[[7]], 'numeric')
  expect_is(sapf_csv_data_nan[[7]], 'numeric')
  expect_is(env_csv_data[['ta']], 'numeric')
  expect_is(env_csv_data_nan[['ta']], 'logical')
})

context('E2. Metadata load')

xlsx_name <- 'foo.xlsx'
csv_name <- 'foo.csv'
csv_name_2 <- 'foo_env.csv'

site_md <- suppressMessages(dl_metadata(xlsx_name, 'site_md'))
stand_md <- dl_metadata(xlsx_name, 'stand_md', si_code_loc = site_md)
species_md <- dl_metadata(xlsx_name, 'species_md', si_code_loc = site_md)
plant_md <- dl_metadata(xlsx_name, 'plant_md', si_code_loc = site_md)
env_md <- dl_metadata(xlsx_name, 'environmental_md', si_code_loc = site_md)

xlsx_name <- 'foo_nan.xlsx'
csv_name <- 'foo_nan.csv'
csv_name_2 <- 'foo_nan_env.csv'

site_md_nan <- suppressMessages(dl_metadata(xlsx_name, 'site_md'))
stand_md_nan <- dl_metadata(xlsx_name, 'stand_md', si_code_loc = site_md)
species_md_nan <- dl_metadata(xlsx_name, 'species_md', si_code_loc = site_md)
plant_md_nan <- dl_metadata(xlsx_name, 'plant_md', si_code_loc = site_md)
env_md_nan <- dl_metadata(xlsx_name, 'environmental_md', si_code_loc = site_md)

test_that('metadata loaded are data frames', {
  expect_is(site_md, 'data.frame')
  expect_is(stand_md, 'data.frame')
  expect_is(species_md, 'data.frame')
  expect_is(plant_md, 'data.frame')
  expect_is(env_md, 'data.frame')
  expect_is(site_md_nan, 'data.frame')
  expect_is(stand_md_nan, 'data.frame')
  expect_is(species_md_nan, 'data.frame')
  expect_is(plant_md_nan, 'data.frame')
  expect_is(env_md_nan, 'data.frame')
})

test_that('metadata errors produces the adequate messages', {
  expect_error(dl_metadata(xlsx_name, 'ste_md'),
               'Provided sheet name is not')
  expect_message(dl_metadata(xlsx_name, 'site_md'),
                 'si_code_loc set to NULL')
  expect_error(dl_metadata('inexistent_file', 'stand_md', si_code_loc = site_md),
               'File does not exist,')
  expect_error(dl_metadata(25, 'stand_md', si_code_loc = site_md),
               'File name is not provided as character')
})
