library(sapfluxnetr)
################################################################################
context('H1. TIMESTAMP format')

good_data <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00", "2003-06-03 00:14:59", "2003-06-03 00:30:00",
  "2003-06-03 00:45:00", "2003-06-03 00:59:59", "2003-06-03 01:15:00",
  "2003-06-03 01:30:00", "2003-06-03 01:44:59", "2003-06-03 02:00:00"
  ), tz = 'Etc/GMT-1'),
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
  "2003-06-03 00:00:00", "2003-06-03 00:14:00", "2003-06-03 00:30:00",
  "2003-06-03 00:45:00", "2003-06-03 00:59:00", "2003-06-03 01:15:00",
  "2003-06-03 01:30:00", "2003-06-03 01:44:00", "2003-06-03 02:00:00"
), tz = 'Etc/GMT-1')

bad_data_1 <- data.frame(TIMESTAMP = c(
  "Wednesday, 23 May, 2015. 10:00", "Wednesday, 23 May, 2015. 10:15",
  "Wednesday, 23 May, 2015. 10:30", "Wednesday, 23 May, 2015. 10:45",
  "Wednesday, 23 May, 2015. 11:00", "Wednesday, 23 May, 2015. 11:15",
  "Wednesday, 23 May, 2015. 11:30", "Wednesday, 23 May, 2015. 11:45",
  "Wednesday, 23 May, 2015. 12:00"
  ),
  Other_var = 1:9,
  stringsAsFactors = FALSE)

bad_1_timestamp <- as.POSIXct(c(
  "2015-05-23 10:00:00", "2015-05-23 10:15:00", "2015-05-23 10:30:00",
  "2015-05-23 10:45:00", "2015-05-23 11:00:00", "2015-05-23 11:15:00",
  "2015-05-23 11:30:00", "2015-05-23 11:45:00", "2015-05-23 12:00:00"
), tz = 'Etc/GMT-1')

bad_data_2 <- data.frame(STAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
)), stringsAsFactors = FALSE)

bad_data_nas <- data.frame(TIMESTAMP = as.POSIXct(c(
  rep(NA, 9)
)), stringsAsFactors = FALSE)

foo_vector <- as.POSIXct(c(
  "2003-06-03 00:00:00", "2003-06-03 00:14:59", "2003-06-03 00:30:00",
  "2003-06-03 00:45:00", "2003-06-03 00:59:59", "2003-06-03 01:15:00",
  "2003-06-03 01:30:00", "2003-06-03 01:44:59", "2003-06-03 02:00:00"
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
  expect_warning(qc_is_timestamp(rep(NA, 5)),
                 'WARNING: TIMESTAMP is all NAs')
  expect_warning(qc_is_timestamp(bad_data_nas),
                 'WARNING: TIMESTAMP is all NAs')
})

test_that('Invisible logicals works', {
  expect_true(qc_is_timestamp(good_data))
  expect_true(qc_is_timestamp(foo_vector))
  expect_false(suppressWarnings(qc_is_timestamp(bad_data_1)))
  expect_false(suppressWarnings(qc_is_timestamp(rep(NA, 5))))
  expect_false(suppressWarnings(qc_is_timestamp(bad_data_nas)))
})

################################################################################
context('H2. TIMESTAMP conversion')

foo_md <- data.frame(
  env_time_zone = "17UTC+01:00, A"
)

suppressMessages(as_good <- qc_as_timestamp(good_data, foo_md))
suppressMessages(as_bad <- qc_as_timestamp(bad_data, foo_md))
suppressMessages(as_bad_1 <- qc_as_timestamp(bad_data_1, foo_md))
suppressMessages(as_vec_good <- qc_as_timestamp(foo_vector, foo_md))

test_that('Results only change the TIMESTAMP, not other variables', {
  expect_identical(as_good, good_data)
  expect_identical(as_bad$Other_var, bad_data$Other_var)
  expect_identical(as_bad_1$Other_var, bad_data_1$Other_var)
  expect_identical(attr(as_vec_good, 'tz'), 'Etc/GMT-1')
})

test_that('TIMESTAMP produced is correct', {
  expect_identical(as_bad$TIMESTAMP, bad_timestamp)
  expect_identical(as_bad_1$TIMESTAMP, bad_1_timestamp)
})

test_that('all NA TIMESTAMPs raise an error', {
  expect_error(suppressWarnings(qc_as_timestamp(bad_data_nas, foo_md)))
  expect_error(suppressWarnings(qc_as_timestamp(c(NA,NA,NA,NA), foo_md)))
})

################################################################################
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

################################################################################
context('H4. Getting timestep')

timestep_pl_good <- data.frame(
  pl_sens_timestep = rep(15, 10)
)

timestep_pl_bad <- data.frame(
  pl_sens_timestep = c(rep(15, 9), 14)
)

timestep_env_good <- data.frame(
  env_timestep = rep(15, 10)
)

timestep_env_bad <- data.frame(
  env_timestep = c(14, rep(15, 9))
)

timestep_bad <- data.frame(
  timestep = rep(15, 10)
)

test_that('argument errors are raised correctly', {
  expect_error(qc_get_timestep(c(1,1,1,1)), 'metadata provided is not a data frame')
  expect_error(qc_get_timestep('tralara'), 'metadata provided is not a data frame')
  expect_error(qc_get_timestep(timestep_bad),
               'Not timestep variables found in metadata provided')
})

test_that('error raised if different timesteps', {
  expect_error(qc_get_timestep(timestep_pl_bad),
               'There are diferent timesteps in the metadata, please check manually')
  expect_error(qc_get_timestep(timestep_env_bad),
               'There are diferent timesteps in the metadata, please check manually')
})

test_that('correct results are showed', {
  expect_identical(qc_get_timestep(timestep_pl_good), 15)
  expect_identical(qc_get_timestep(timestep_env_good), 15)
})

################################################################################
context('H5. Time intervals of trees and environmental variables')

intervals_data <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
)),
Tree_1 = 1:9,
Tree_2 = c(NA, NA, NA, NA, 5:9),
Tree_3 = c(1:8, NA),
Tree_4 = rep(NA, 9),
Tree_5 = c(1:3, NA, NA, 6:9),
stringsAsFactors = FALSE)

res_intervals_data <- qc_time_interval(intervals_data)

test_that('Time intervals are correctly calculated', {
  expect_equal(res_intervals_data[[2, 2]], intervals_data[[1, 1]])
  expect_equal(res_intervals_data[[2, 3]], intervals_data[[9, 1]])
  expect_equal(res_intervals_data[[3, 2]], intervals_data[[5, 1]])
  expect_equal(res_intervals_data[[3, 3]], intervals_data[[9, 1]])
  expect_equal(res_intervals_data[[4, 2]], intervals_data[[1, 1]])
  expect_equal(res_intervals_data[[4, 3]], intervals_data[[8, 1]])
  expect_equal(res_intervals_data[[5, 2]], as.POSIXct(NA))
  expect_equal(res_intervals_data[[5, 3]], as.POSIXct(NA))
  expect_equal(res_intervals_data[[6, 2]], intervals_data[[1, 1]])
  expect_equal(res_intervals_data[[6, 3]], intervals_data[[9, 1]])
})

intervals_data$TIMESTAMP <- NULL

test_that('Errors are raised correctly', {
  expect_error(qc_time_interval(intervals_data),
               'data has not a TIMESTAMP variable')
  expect_error(qc_time_interval('intervals_data'),
               'data provided is not a data frame')
})

intervals_data <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
)),
Tree_1 = 1:9,
Tree_2 = c(NA, NA, NA, NA, 5:9),
Tree_3 = c(1:8, NA),
Tree_4 = rep(NA, 9),
Tree_5 = c(1:3, NA, NA, 6:9),
stringsAsFactors = FALSE)

intervals_env_data <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
)),
ta = 1:9,
precip = c(NA, NA, NA, NA, 5:9),
incoming = c(1:8, NA),
ws = rep(NA, 9),
hrs = c(1:3, NA, NA, 6:9),
stringsAsFactors = FALSE)

intervals_plot <- qc_timestamp_concordance(intervals_data, intervals_env_data,
                                           plot = TRUE)

intervals_plot_2 <- qc_timestamp_concordance(
  sapf_intervals = qc_time_interval(intervals_data),
  env_intervals = qc_time_interval(intervals_env_data),
  plot = TRUE
)

test_that('Plots are plots', {
  expect_is(intervals_plot, 'ggplot')
  expect_is(intervals_plot_2, 'ggplot')
  expect_equal(intervals_plot, intervals_plot_2)
})

intervals_plot <- qc_timestamp_concordance(intervals_data, intervals_env_data,
                                           plot = FALSE)

intervals_plot_2 <- qc_timestamp_concordance(
  sapf_intervals = qc_time_interval(intervals_data),
  env_intervals = qc_time_interval(intervals_env_data),
  plot = FALSE
)

test_that('data frames are data frames', {
  expect_is(intervals_plot, 'data.frame')
  expect_is(intervals_plot_2, 'data.frame')
  expect_identical(intervals_plot, intervals_plot_2)
})

################################################################################
context('H6. TIMESTAMP NAs')

data_wo_nas <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
)),
Tree_1 = 1:9,
Tree_2 = c(NA, NA, NA, NA, 5:9),
Tree_3 = c(1:8, NA),
Tree_4 = rep(NA, 9),
Tree_5 = c(1:3, NA, NA, 6:9),
stringsAsFactors = FALSE)

data_w_nas <- data.frame(TIMESTAMP = as.POSIXct(c(
  "2003-06-03 00:00:00 UTC", NA, NA,
  "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
  "2003-06-03 01:30:00 UTC", NA, "2003-06-03 02:00:00 UTC"
)),
Tree_1 = 1:9,
Tree_2 = c(NA, NA, NA, NA, 5:9),
Tree_3 = c(1:8, NA),
Tree_4 = rep(NA, 9),
Tree_5 = c(1:3, NA, NA, 6:9),
stringsAsFactors = FALSE)

res_wo_nas <- qc_timestamp_nas(data_wo_nas)
res_w_nas <- qc_timestamp_nas(data_w_nas)


test_that('No NAs returns TRUE', {
  expect_true(qc_timestamp_nas(data_wo_nas))
  expect_true(res_wo_nas)
})

test_that('NAs returns data frame', {
  expect_is(qc_timestamp_nas(data_w_nas), 'data.frame')
  expect_is(res_w_nas, 'data.frame')
  expect_length(res_w_nas$row_number, 3)
})

################################################################################
context('H7. Solar time conversion')

intervals_data <- data.frame(TIMESTAMP = lubridate::parse_date_time(c(
  "2010-04-29 22:30:00", "2010-04-29 22:45:00", "2010-04-29 23:00:00",
  "2010-04-29 23:15:00", "2010-04-29 23:30:00", "2010-04-29 23:45:00",
  "2010-04-30 00:00:00", "2010-04-30 00:15:00", "2010-04-30 00:30:00"
),
orders = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-1"
),
Tree_1 = 1:9,
Tree_2 = c(NA, NA, NA, NA, 5:9),
Tree_3 = c(1:8, NA),
Tree_4 = rep(NA, 9),
Tree_5 = c(1:3, NA, NA, 6:9),
stringsAsFactors = FALSE)

site_metadata <- data.frame(
  si_country = 'ESP',
  si_lat = 41.33262995,
  si_long = 1.0144288
)

test_that('Errors are raised correctly', {
  expect_error(qc_solar_timestamp(intervals_data, 'site_metadata'),
               'data and/or site_md are not data frames')
  expect_error(qc_solar_timestamp(subset(intervals_data, select=-TIMESTAMP), site_metadata),
               'data has not a TIMESTAMP variable')
  expect_error(qc_solar_timestamp(intervals_data, subset(site_metadata, select=-si_lat)),
               'site_md have not the needed variables.')
  expect_error(qc_solar_timestamp(intervals_data, site_metadata, type = 'other'),
               'type = "')
})

results_mean <- as.POSIXct(
  c("2010-04-29 21:34:03 UTC", "2010-04-29 21:49:03 UTC", "2010-04-29 22:04:03 UTC",
    "2010-04-29 22:19:03 UTC", "2010-04-29 22:34:03 UTC", "2010-04-29 22:49:03 UTC",
    "2010-04-29 23:04:03 UTC", "2010-04-29 23:19:03 UTC", "2010-04-29 23:34:03 UTC")
)

results_apparent <- as.POSIXct(
  c("2010-04-29 21:36:48 UTC", "2010-04-29 21:51:48 UTC", "2010-04-29 22:06:48 UTC",
    "2010-04-29 22:21:48 UTC", "2010-04-29 22:36:48 UTC", "2010-04-29 22:51:48 UTC",
    "2010-04-29 23:06:48 UTC", "2010-04-29 23:21:48 UTC", "2010-04-29 23:36:48 UTC")
)

test_that('conversion is made correctly', {
  expect_equal(
    qc_solar_timestamp(intervals_data, site_metadata, type = 'mean')$TIMESTAMP,
    results_mean, tolerance = 1
  )
  expect_equal(
    qc_solar_timestamp(intervals_data, site_metadata)$TIMESTAMP,
    results_apparent, tolerance = 1
  )
})

test_that('output is a data frame', {
  expect_is(qc_solar_timestamp(intervals_data, site_metadata), 'data.frame')
})
