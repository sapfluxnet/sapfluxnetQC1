library(testthat)

context('L1. Sapflow ranges tests')

set.seed(25)

foo_data <- data.frame(
  TIMESTAMP = rep('No date', 100),
  Tree_1 = rnorm(100, 250, 125),
  Tree_2 = rnorm(100, 65, 5),
  Tree_3 = rnorm(100, 25, 15),
  Tree_4 = rnorm(100, 400, 125),
  Tree_5 = rnorm(100, 200, 50)
)

set.seed(NULL)

foo_res <- qc_sapf_range_check(foo_data)

test_that('Arguments checks works', {
  expect_error(qc_sapf_range_check('foo_data'),
               'data provided is not a data frame')
})

test_that('Function works as expected', {
  expect_is(foo_res, 'data.frame')
  expect_length(foo_res[,1], 20)
  expect_length(foo_res[foo_res$Plant == 'Tree_1', 'Value'], 2)
  expect_length(foo_res[foo_res$Plant == 'Tree_2', 'Value'], 0)
  expect_length(foo_res[foo_res$Plant == 'Tree_3', 'Value'], 2)
  expect_length(foo_res[foo_res$Plant == 'Tree_4', 'Value'], 16)
  expect_length(foo_res[foo_res$Plant == 'Tree_5', 'Value'], 0)
  expect_length(foo_res[foo_res$Value > 533, 'Value'], 17)
  expect_length(foo_res[foo_res$Value < -10, 'Value'], 3)
})
