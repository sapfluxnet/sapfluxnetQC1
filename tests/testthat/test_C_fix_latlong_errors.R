library(sapfluxnetQC1)

context('C1. Fix latlong errors arguments')

foo_data <- data.frame(
  si_long = c(84.4800, 8.6100, 174.4924, 7.0000, 48.0000),
  si_lat = c(10.1800, 39.9000, -36.7959, 46.0000, 23.0000),
  si_country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
  si_name = c('bla', 'ble', 'bli', 'blo', 'blu'),
  is_inside_country = c(FALSE, TRUE, TRUE, FALSE, FALSE)
)

test_that('argument errors are correct', {

  expect_error(qc_fix_latlong_errors(foo_data, 'foo_folder'),
               'maps_folder location does not exist')
  expect_error(qc_fix_latlong_errors(c(1,2,3)),
               'Provided data object is not a data.frame')
  expect_error(qc_fix_latlong_errors(foo_data[,2:5]), 'There is no longitude variable')
  expect_error(qc_fix_latlong_errors(foo_data[,c(1, 3:5)]), 'There is no latitude variable')
  expect_error(qc_fix_latlong_errors(foo_data[,c(1:2, 4:5)]), 'There is no country variable')
  expect_error(qc_fix_latlong_errors(foo_data[,c(1:3, 5)]), 'There is no site_name variable')
  expect_error(qc_fix_latlong_errors(foo_data[,c(1:4)]), 'There is no is_inside_country variable')
})

context('C2. Fix latlong errors result')

test_that('results are a dataframe', {
  expect_true(is.data.frame(qc_fix_latlong_errors(foo_data)))
})

test_that('results are correct (without special countries)', {
  expect_message(qc_fix_latlong_errors(foo_data), '0 latitude sign errors fixed. 2 longitude sign errors fixed. 1 unable to fix due to country borders sharing positive and negative coordinates.\n')
})

test_that('results are correct (with special countries', {
  expect_message(qc_fix_latlong_errors(foo_data), '0 latitude sign errors fixed. 2 longitude sign errors fixed. 1 unable to fix due to country borders sharing positive and negative coordinates.\n')
})

file.remove('CRI_adm0.rds', 'ITA_adm0.rds', 'NZL_adm0.rds', 'CHE_adm0.rds',
            'BRA_adm0.rds', 'ITA_ble.pdf', 'Rplots.pdf')

context('C3. qc_coordinates wrapper function')

foo_data_2 <- data.frame(
  si_long = c(-5.797067),
  si_lat = c(37.249105),
  si_country = c('ESP'),
  si_name = c('San')
)

foo_data_2_bad <- data.frame(
  si_long = c(5.797067),
  si_lat = c(37.249105),
  si_country = c('ESP'),
  si_name = c('San')
)

foo_data_2_really_bad <- data.frame(
  si_long = c(-45.797067),
  si_lat = c(68.249105),
  si_country = c('ESP'),
  si_name = c('San')
)

foo_res_2 <- suppressMessages(qc_coordinates(foo_data_2))
foo_res_2_bad <- suppressMessages(qc_coordinates(foo_data_2_bad))
foo_res_2_really_bad <- suppressMessages(qc_coordinates(foo_data_2_really_bad))

test_that('results are data frames', {
  expect_true(is.data.frame(foo_res_2))
  expect_true(is.data.frame(foo_res_2_bad))
  expect_true(is.data.frame(foo_res_2_really_bad))
})

test_that('results are correct', {
  expect_true(foo_res_2$is_inside_country)
  expect_true(foo_res_2_bad$is_inside_country)
  expect_true(!foo_res_2_really_bad$is_inside_country)
})

file.remove('ESP_adm0.rds')
