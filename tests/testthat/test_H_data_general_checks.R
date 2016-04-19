library(sapfluxnetr)

context('H1. TIMESTAMP format')

good_data <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
  )), stringsAsFactors = FALSE)

bad_data_1 <- data.frame(TIMESTAMP = c(
  "Miercoles, 23 de Mayo, 2015. 10:00", "Miercoles, 23 de Mayo, 2015. 10:15",
  "Miercoles, 23 de Mayo, 2015. 10:30", "Miercoles, 23 de Mayo, 2015. 10:45",
  "Miercoles, 23 de Mayo, 2015. 11:00", "Miercoles, 23 de Mayo, 2015. 11:15",
  "Miercoles, 23 de Mayo, 2015. 11:30", "Miercoles, 23 de Mayo, 2015. 11:45",
  "Miercoles, 23 de Mayo, 2015. 12:00"
  ), stringsAsFactors = FALSE)

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
  expect_error(qc_is_timestamp(foo_vector), 'Data provided is not a data frame')
  expect_error(qc_is_timestamp(bad_data_2),
               'TIMESTAMP variable is missing in the data provided')
})

test_that('Message or warning are raised correctly', {
  expect_message(qc_is_timestamp(good_data),
                 'TIMESTAMP is in the correct format')
  expect_warning(qc_is_timestamp(bad_data_1),
                 'WARNING: TIMESTAMP is NOT in the correct format')
})

context('H2. TIMESTAMP errors')

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
