library(sapfluxnetr)

context('I1. qc_get_pl_md')

suppressMessages(pl_data <- dl_metadata('foo.xlsx', 'plant_md'))
pl_data_bad <- pl_data
pl_data_bad$pl_code <- NULL

test_that('argument checks work', {
  expect_error(qc_get_pl_md(c('a','b','c')),
               'Provided pl_data object is not a data frame')
  expect_error(qc_get_pl_md(pl_data_bad),
               'pl_code variable is missing from pl_data')
})

test_that('result is a data frame with the correct variables', {
  expect_is(qc_get_pl_md(pl_data), 'data.frame')
  expect_true(all(names(qc_get_pl_md(pl_data) %in% c(
    'pl_code', 'pl_sap_units', 'pl_sapw_area', 'pl_leaf_area',
    'pl_dbh', 'pl_sapw_depth', 'pl_bark_thick', 'pl_sapw_area_est'
  ))))
})

context('I2. qc_sapw_area_calculator')

pl_data_bad_2 <- pl_data
pl_data_bad_2$pl_sapw_area <- NA

pl_data_bad_3 <- pl_data_bad_2
pl_data_bad_3$pl_bark_thick <- NA

pl_data_bad_4 <- pl_data
pl_data_bad_4$pl_sapw_depth <- NULL

pl_data_bad_5 <- pl_data_bad_2
pl_data_bad_5$pl_dbh <- NA

pl_data_bad_6 <- pl_data_bad_2
pl_data_bad_6$pl_sapw_depth[c(1,2,3,6,7)] <- NA

test_that('argument checks work', {
  expect_error(qc_sapw_area_calculator(c('a','b','c')),
               'Provided pl_vars object is not a data frame')
  expect_error(qc_sapw_area_calculator(pl_data_bad_4),
               'Provided pl_vars object has not the needed variables')
})

test_that('function works', {
  # results are data frames
  expect_is(qc_sapw_area_calculator(pl_data), 'data.frame')
  expect_is(qc_sapw_area_calculator(pl_data_bad_2), 'data.frame')
  # messages appears when they are needed
  expect_message(qc_sapw_area_calculator(pl_data_bad_3),
                 'Estimate of sapwood area must be taken with caution')
  expect_message(qc_sapw_area_calculator(pl_data_bad_5),
                 'can not be calculated. Returning NA.')
  # results are correct
  expect_equal(qc_sapw_area_calculator(pl_data)$pl_sapw_area,
               qc_sapw_area_calculator(pl_data)$pl_sapw_area_est)
  expect_equal(qc_sapw_area_calculator(pl_data_bad_2)$pl_sapw_area_est,
               pl_data$pl_sapw_area, tolerance = 5)
  expect_true(all(is.na(qc_sapw_area_calculator(pl_data_bad_5)$pl_sapw_area_est)))
  expect_equal(sum(is.na(qc_sapw_area_calculator(pl_data_bad_6)$pl_sapw_area_est)), 5)
})

