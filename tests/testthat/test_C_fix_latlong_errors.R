library(sapfluxnetr)

context('C1. Fix latlong errors arguments')

foo_data <- data.frame(
  longitude = c(84.4800, 8.6100, 174.4924, 7.0000, 48.0000),
  latitude = c(10.1800, 39.9000, -36.7959, 46.0000, 23.0000),
  country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
  site_name = c('bla', 'ble', 'bli', 'blo', 'blu'),
  is_inside_country = c(FALSE, TRUE, TRUE, FALSE, FALSE)
)

test_that('argument errors are correct', {

  expect_error(fix_latlong_errors(foo_data, 'foo_folder'),
               'maps_folder location does not exist')
  expect_error(fix_latlong_errors(c(1,2,3)),
               'Provided data object is not a data.frame')
  expect_error(fix_latlong_errors(foo_data[,2:5]), 'There is no longitude variable')
  expect_error(fix_latlong_errors(foo_data[,c(1, 3:5)]), 'There is no latitude variable')
  expect_error(fix_latlong_errors(foo_data[,c(1:2, 4:5)]), 'There is no country variable')
  expect_error(fix_latlong_errors(foo_data[,c(1:3, 5)]), 'There is no site_name variable')
  expect_error(fix_latlong_errors(foo_data[,c(1:4)]), 'There is no is_inside_country variable')
})

context('C2. Fix latlong errors result')

test_that('results are a dataframe', {
  expect_true(is.data.frame(fix_latlong_errors(foo_data)))
})

test_that('results are correct (without special countries)', {
  expect_message(fix_latlong_errors(foo_data), '0 latitude sign errors fixed.\n2 longitude sign errors fixed.\n1 unable to fix due to country borders sharing positive and negative coordinates.\n\n')
})

test_that('results are correct (with special countries', {
  expect_message(fix_latlong_errors(foo_data), '0 latitude sign errors fixed.\n2 longitude sign errors fixed.\n1 unable to fix due to country borders sharing positive and negative coordinates.\n\n')
})

file.remove('CRI_adm0.rds', 'ITA_adm0.rds', 'NZL_adm0.rds', 'CHE_adm0.rds',
            'BRA_adm0.rds', 'ITA_ble.pdf', 'Rplots.pdf')
