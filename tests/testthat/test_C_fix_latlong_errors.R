library(sapfluxnetr)

context('C1. Fix latlong errors arguments')

foo_data <- data.frame(
  longitude = rnorm(5),
  latitude = rnorm(5),
  country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
  site_name = c('bla', 'ble', 'bli', 'blo', 'blu'),
  is_inside_country = c(FALSE, TRUE, FALSE, FALSE, TRUE)
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

test_that('results are a dataframe', {
  # expect_true(is.data.frame())
})

file.remove('CRI_adm0.rds', 'ITA_adm0.rds', 'NZL_adm0.rds', 'CHE_adm0.rds',
            'BRA_adm0.rds', 'ITA_ble.pdf', 'Rplots.pdf')
