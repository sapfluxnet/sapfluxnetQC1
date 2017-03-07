library(sapfluxnetQC1)

context('L1. Ranges')

load('FOO.RData')
foo_sapf <- get_sapf(FOO)
foo_flags <- get_sapf_flags(FOO)
foo_plant_md <- get_plant_md(FOO)


test_that('ranges dictionary is created ok', {
  expect_is(qc_range_dic(), 'list')
  expect_length(qc_range_dic(), 10)
  expect_identical(names(qc_range_dic()),
                   c('ta', 'rh', 'vpd', 'sw_in', 'ppdf_in', 'netrad',
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


