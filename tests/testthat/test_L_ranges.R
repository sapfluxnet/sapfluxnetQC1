library(sapfluxnetQC1)

context('L1. Ranges')

# needed data
load('FOO.RData')
foo_sapf <- get_sapf(FOO)
foo_sapf_flags <- get_sapf_flags(FOO)
foo_plant_md <- get_plant_md(FOO)
foo_env <- get_env(FOO)
foo_env_flags <- get_env_flags(FOO)

# env modified data (each variable must have 10 values out of range)
foo_env[1:10, 'ta'] <- c(-30.1, -35, -40, -56, -60,
                         50.001, 55, 67, 78, 101)
foo_env[1:10, 'rh'] <- c(-1, -5, -40, -56, -60,
                         101, 110, 103, 125, 1350)
foo_env[1:10, 'vpd'] <- c(-1, -5, -40, -56, -60,
                          40.001, 55, 67, 78, 101)
foo_env[1:10, 'ppfd_in'] <- c(-1, -5, -40, -56, -60,
                              2400.001, 2455, 3167, 7788, 101090)
foo_env[1:10, 'ws'] <- c(-1, -5, -40, -56, -60,
                         45.001, 55, 67, 78, 101)
foo_env[1:10, 'precip'] <- c(-1, -5, -40, -56, -60,
                             250.001, 255, 367, 478, 6101)

foo_env$sw_in <- c(-1, -5, -40, -56, -60,
                   1362.001, 2255, 1367, 1478, 6101,
                   rnorm(47606, 500, 5))

foo_env$netrad <- c(-281, -285, -340, -456, -600,
                    280.001, 295, 1367, 1478, 6101,
                    rnorm(47606, 0, 10))

# env_flags modified to add the variables not listed at first
foo_env_flags$sw_in <- ''
foo_env_flags$netrad <- ''

test_that('ranges dictionary is created ok', {
  expect_is(qc_range_dic(), 'list')
  expect_length(qc_range_dic(), 10)
  expect_identical(names(qc_range_dic()),
                   c('ta', 'rh', 'vpd', 'sw_in', 'ppfd_in', 'netrad',
                     'ws', 'precip', 'sapf_sapw', 'sapf_tree'))
  expect_length(qc_range_dic()[[1]], 2)
  expect_length(qc_range_dic()[[2]], 2)
  expect_length(qc_range_dic()[[3]], 2)
  expect_length(qc_range_dic()[[4]], 2)
  expect_length(qc_range_dic()[[5]], 2)
  expect_length(qc_range_dic()[[6]], 2)
  expect_length(qc_range_dic()[[7]], 2)
  expect_length(qc_range_dic()[[8]], 2)
  expect_length(qc_range_dic()[[9]], 2)
  expect_length(qc_range_dic()[[10]], 2)
})

test_that('env ranges are checked correctly', {
  foo_env_test <- qc_env_ranges(foo_env, foo_env_flags)

  expect_equal(sum(as.matrix(foo_env_test) == 'RANGE_WARN'), 80)
  expect_equal(sum(as.matrix(foo_env_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,4]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,5]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,6]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,7]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,8]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,9]) == 'RANGE_WARN'), 10)
})
