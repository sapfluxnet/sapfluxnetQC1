library(sapfluxnetr)

context('B1. Coordinates argument checks')

test_that('argument errors are correct', {

  foo_data <- data.frame(
    si_long = rnorm(5),
    si_lat = rnorm(5),
    si_country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
    si_name = c('bla', 'ble', 'bli', 'blo', 'blu')
  )

  expect_error(check_coordinates(foo_data, 'foo_folder'),
               'Maps folder does not exist')
  expect_error(check_coordinates(c(1,2,3)),
               'Provided data object is not a data.frame')
  expect_error(check_coordinates(foo_data[,2:4]), 'There is no longitude variable')
  expect_error(check_coordinates(foo_data[,c(1, 3:4)]), 'There is no latitude variable')
  expect_error(check_coordinates(foo_data[,c(1:2, 4)]), 'There is no country variable')
  expect_error(check_coordinates(foo_data[,c(1:3)]), 'There is no site_name variable')
})

context('B2. Coordinates check')

test_that('checking is correct', {

  foo_data <- data.frame(
    si_long = c(-84, rnorm(4, 0, 0.1)),
    si_lat = c(10, rnorm(4, 0, 0.1)),
    si_country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
    si_name = c('bla', 'ble', 'bli', 'blo', 'blu')
  )
  download_maps(foo_data)

  expect_message(check_coordinates(foo_data[2:5,]), '4 wrong coordinates in data')
  expect_message(check_coordinates(foo_data[1,]), '1 correct coordinates in data')
})

context('B3. Plotting wrong coordinates and report presence')

test_that('wrong coordinates are plotted correctly', {

  foo_data <- data.frame(
    si_long = c(-84, rnorm(4, 0, 0.1)),
    si_lat = c(10, rnorm(4, 0, 0.1)),
    si_country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
    si_name = c('bla', 'ble', 'bli', 'blo', 'blu')
  )
  foo_report <- check_coordinates(foo_data[1:2,], plot = TRUE)

  expect_false(file_test("-f", 'CRI_bla.pdf'))
  expect_true(file_test("-f", 'ITA_ble.pdf'))
})

test_that('report is present and is a data.frame', {

  foo_data <- data.frame(
    si_long = c(-84, rnorm(4, 0, 0.1)),
    si_lat = c(10, rnorm(4, 0, 0.1)),
    si_country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
    si_name = c('bla', 'ble', 'bli', 'blo', 'blu')
  )
  foo_report <- check_coordinates(foo_data[1,])

  expect_is(foo_report, 'data.frame')
})
