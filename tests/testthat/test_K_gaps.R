library(testthat)

context('K1. Mind the gap')

foo_data <- data.frame(
  TIMESTAMP = as.POSIXct(c(
    "2003-06-03 00:00:00 UTC", "2003-06-03 00:14:59 UTC", "2003-06-03 00:30:00 UTC",
    "2003-06-03 00:45:00 UTC", "2003-06-03 00:59:59 UTC", "2003-06-03 01:15:00 UTC",
    "2003-06-03 01:30:00 UTC", "2003-06-03 01:44:59 UTC", "2003-06-03 02:00:00 UTC"
  ), tz = 'Etc/GMT+0'),
  A = c(1,2,3,NA,NA,NA,7,8,9),
  B = c(1,2,NA,NA,5,6,NA,NA,9),
  C = c(NA,NA,3,4,5,6,7,8,NA),
  D = c(NA,NA,3,4,5,6,7,8,9),
  E = c(1,2,3,4,NA,6,NA,8,9)
)

foo_res <- qc_mind_the_gap(foo_data)
foo_res_trim <- qc_mind_the_gap(foo_data, trim = TRUE)

test_that('Errors are raised correctly', {
  expect_error(qc_mind_the_gap('foo_data'), 'Data is not a data frame')
  expect_error(qc_mind_the_gap(foo_data[,-1]), 'TIMESTAMP variable is missing in data')
})

test_that('Results are correct', {
  expect_is(foo_res, 'data.frame')
  expect_is(foo_res_trim, 'data.frame')
  expect_equal(length(foo_res[,1]), 8)
  expect_equal(length(foo_res_trim[,1]), 4)
  expect_false(any(foo_res$gap_coverage >= 100))
  expect_false(any(foo_res_trim$gap_coverage >= 100))
  expect_identical(attr(foo_data$TIMESTAMP, 'tzone'),
                   attr(foo_res$gap_start, 'tzone'))
  expect_identical(attr(foo_data$TIMESTAMP, 'tzone'),
                   attr(foo_res_trim$gap_start, 'tzone'))
})

context('K2. Plot the gap')

foo_plot <- vis_plot_the_gap(foo_res, type = 'gap_interval')
foo_plot2 <- vis_plot_the_gap(foo_res_trim, type = 'gap_coverage')

test_that('plots are plots', {
  expect_is(foo_plot, 'ggplot')
  expect_is(foo_plot2, 'ggplot')
})
