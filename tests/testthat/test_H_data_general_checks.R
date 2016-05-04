library(sapfluxnetr)

context('H1. TIMESTAMP format')

good_data <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
  )),
  Other_var = 1:9,
  stringsAsFactors = FALSE)

bad_data <- data.frame(TIMESTAMP = c(
  "2003-06-03 00:00", "2003-06-03 00:14", "2003-06-03 00:30",
  "2003-06-03 00:45", "2003-06-03 00:59", "2003-06-03 01:15",
  "2003-06-03 01:30", "2003-06-03 01:44", "2003-06-03 02:00"
  ),
  Other_var = 1:9,
  stringsAsFactors = FALSE)

bad_timestamp <- as.POSIXct(c(
  "2003-06-03 00:00:00 CEST", "2003-06-03 00:14:00 CEST", "2003-06-03 00:30:00 CEST",
  "2003-06-03 00:45:00 CEST", "2003-06-03 00:59:00 CEST", "2003-06-03 01:15:00 CEST",
  "2003-06-03 01:30:00 CEST", "2003-06-03 01:44:00 CEST", "2003-06-03 02:00:00 CEST"
), tz = 'UTC')

bad_data_1 <- data.frame(TIMESTAMP = c(
  "Miercoles, 23 de Mayo, 2015. 10:00", "Miercoles, 23 de Mayo, 2015. 10:15",
  "Miercoles, 23 de Mayo, 2015. 10:30", "Miercoles, 23 de Mayo, 2015. 10:45",
  "Miercoles, 23 de Mayo, 2015. 11:00", "Miercoles, 23 de Mayo, 2015. 11:15",
  "Miercoles, 23 de Mayo, 2015. 11:30", "Miercoles, 23 de Mayo, 2015. 11:45",
  "Miercoles, 23 de Mayo, 2015. 12:00"
  ),
  Other_var = 1:9,
  stringsAsFactors = FALSE)

bad_1_timestamp <- as.POSIXct(c(
  "2015-05-23 10:00:00 CEST", "2015-05-23 10:15:00 CEST", "2015-05-23 10:30:00 CEST",
  "2015-05-23 10:45:00 CEST", "2015-05-23 11:00:00 CEST", "2015-05-23 11:15:00 CEST",
  "2015-05-23 11:30:00 CEST", "2015-05-23 11:45:00 CEST", "2015-05-23 12:00:00 CEST"
), tz = 'UTC')

bad_data_2 <- data.frame(STAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
)), stringsAsFactors = FALSE)

foo_vector <- as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
  ))

test_that('Arguments raise the correct errors', {
  expect_error(qc_is_timestamp(matrix(1:10)), 'Data provided is not a data frame')
  expect_error(qc_is_timestamp(bad_data_2),
               'TIMESTAMP variable is missing in the data provided')
})

test_that('Message or warning are raised correctly', {
  expect_message(qc_is_timestamp(good_data),
                 'TIMESTAMP is in the correct format')
  expect_message(qc_is_timestamp(foo_vector),
                 'TIMESTAMP is in the correct format')
  expect_warning(qc_is_timestamp(bad_data_1),
                 'WARNING: TIMESTAMP is NOT in the correct format')
})

test_that('Invisible logicals works', {
  expect_true(qc_is_timestamp(good_data))
  expect_true(qc_is_timestamp(foo_vector))
  expect_false(qc_is_timestamp(bad_data_1))
})

context('H2. TIMESTAMP conversion')

suppressMessages(as_good <- qc_as_timestamp(good_data))
suppressMessages(as_bad <- qc_as_timestamp(bad_data))
suppressMessages(as_bad_1 <- qc_as_timestamp(bad_data_1))
suppressMessages(as_vec_good <- qc_as_timestamp(foo_vector))

test_that('Results only change the TIMESTAMP, not other variables', {
  expect_identical(as_good, good_data)
  expect_identical(as_bad$Other_var, bad_data$Other_var)
  expect_identical(as_bad_1$Other_var, bad_data_1$Other_var)
  expect_identical(as_vec_good, foo_vector)
})

test_that('TIMESTAMP produced is correct', {
  expect_identical(as_bad$TIMESTAMP, bad_timestamp)
  expect_identical(as_bad_1$TIMESTAMP, bad_1_timestamp)
})

context('H3. TIMESTAMP errors')

good_data_2 <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
)), stringsAsFactors = FALSE)

bad_data_3 <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 01:44:59 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
)), stringsAsFactors = FALSE)

res <- qc_timestamp_errors(good_data_2, 15)
res_w_err <- qc_timestamp_errors(bad_data_3, 15)

test_that('Arguments raise the correct errors', {
  expect_error(qc_timestamp_errors(foo_vector, 15), 'Data provided is not a data frame')
  expect_error(qc_timestamp_errors(bad_data_2, 15),
               'TIMESTAMP variable is missing in the data provided')
  expect_error(qc_timestamp_errors(good_data, '15'),
               'Provided timestep is not numeric')
})

test_that('Result is a data frame', {
  expect_is(res, 'data.frame')
})

test_that('Results are correct', {
  expect_equal(nrow(res), 0, tolerance = 0)
  expect_equal(nrow(res_w_err), 2, tolerance = 0)
})
