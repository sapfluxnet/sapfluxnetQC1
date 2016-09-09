library(sapfluxnetr)

context('I1. qc_get_sapw_md')

suppressMessages(pl_data <- dl_metadata('foo.xlsx', 'plant_md'))
pl_data_bad <- pl_data
pl_data_bad$pl_code <- NULL

test_that('argument checks work', {
  expect_error(qc_get_sapw_md(c('a','b','c')),
               'Provided pl_data object is not a data frame')
  expect_error(suppressWarnings(qc_get_sapw_md(pl_data_bad)),
               'pl_code variable is missing from pl_data')
})

test_that('result is a data frame with the correct variables', {
  expect_is(qc_get_sapw_md(pl_data), 'data.frame')
  expect_true(all(names(qc_get_sapw_md(pl_data) %in% c(
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

context('I3. Unit conversion')

test_that('argument checks works', {
  expect_error(qc_sapw_conversion('not a data frame', qc_get_sapw_md(pl_data), 'plant'),
               'data and/or pl_metadata objects are not data frames')
  expect_error(qc_sapw_conversion(dl_data('foo.xlsx', 'sapflow_hd'),
                                  'not a data frame', 'plant'),
               'data and/or pl_metadata objects are not data frames')
  expect_error(
    qc_sapw_conversion(dl_data('foo.xlsx', 'sapflow_hd'), qc_get_sapw_md(pl_data), 25),
    'output_units value is not a character vector'
  )
  expect_error(
    qc_sapw_conversion(dl_data('foo.xlsx', 'sapflow_hd'), qc_get_sapw_md(pl_data), '25'),
    'output_units = "'
  )
})

test_data <- data.frame(TIMESTAMP = c(1, 2, 3, 4, 5, 6),
                        pl_cm_cm_h = c(1, 2, 5, 10, 100, 1000),
                        pl_cm_m_s = c(2.7778, 5.5556, 13.8889, 27.7778, 277.7778, 2777.7778),
                        pl_dm_dm_h = c(0.1, 0.2, 0.5, 1, 10, 100),
                        pl_dm_dm_s = c(2.777778e-05, 5.555556e-05, 0.0001388889, 0.0002777778, 0.002777778, 0.02777778),
                        pl_mm_mm_s = c(0.002777778, 0.005555556, 0.01388889, 0.02777778, 0.2777778, 2.777778),
                        pl_g_m_s = c(2.7778, 5.5556, 13.8889, 27.7778, 277.7778, 2777.7778),
                        pl_kg_m_h = c(10, 20, 50, 100, 1000, 10000),
                        pl_kg_m_s = c(0.002777778, 0.005555556, 0.01388889, 0.02777778, 0.2777778, 2.777778),
                        pl_cm_s = c(0.05555556, 0.1111111, 0.2777778, 0.5555556, 5.555556, 55.55556),
                        pl_cm_h = c(200, 400, 1000, 2000, 20000, 200000),
                        pl_dm_h = c(0.2, 0.4, 1, 2, 20, 200),
                        pl_g_h = c(200, 400, 1000, 2000, 20000, 200000),
                        pl_kg_h = c(0.2, 0.4, 1, 2, 20, 200))

test_sapw_md <- data.frame(pl_code = c('pl_cm_cm_h', 'pl_cm_m_s', 'pl_dm_dm_h',
                                       'pl_dm_dm_s', 'pl_mm_mm_s', 'pl_g_m_s',
                                       'pl_kg_m_h', 'pl_kg_m_s', 'pl_cm_s',
                                       'pl_cm_h', 'pl_dm_h', 'pl_g_h', 'pl_kg_h'),
                           pl_sapw_area = rep(200, 13),
                           pl_leaf_area = rep(14, 13),
                           pl_sap_units = c('“cm3 cm-2 h-1”', '“cm3 m-2 s-1”',
                                            '“dm3 dm-2 h-1”', '“dm3 dm-2 s-1”',
                                            '“mm3 mm-2 s-1”', '“g m-2 s-1”',
                                            '“kg m-2 h-1”', '“kg m-2 s-1”',
                                            '“cm3 s-1”', '“cm3 h-1”', '“dm3 h-1”',
                                            '“g h-1”', '“kg h-1”'))

test_expected_sapw <- c(1, 2, 5, 10, 100, 1000)
test_expected_plant <- c(200, 400, 1000, 2000, 20000, 200000)
test_expected_leafarea <- c(0.0014, 0.0029, 0.0071, 0.0143, 0.1429, 1.4288)

test_results_plant <- round(qc_sapw_conversion(test_data, test_sapw_md,
                                         output_units = 'plant'), 1)
test_results_sapw <- round(qc_sapw_conversion(test_data, test_sapw_md,
                                         output_units = 'sapwood'), 1)
test_results_leafarea <- round(qc_sapw_conversion(test_data, test_sapw_md,
                                         output_units = 'leaf'), 4)

test_that('conversion is made correctly', {
  # plant
  expect_equal(test_results_plant$pl_cm_cm_h, test_expected_plant)
  expect_equal(test_results_plant$pl_cm_m_s, test_expected_plant)
  expect_equal(test_results_plant$pl_dm_dm_h, test_expected_plant)
  expect_equal(test_results_plant$pl_dm_dm_s, test_expected_plant)
  expect_equal(test_results_plant$pl_mm_mm_s, test_expected_plant)
  expect_equal(test_results_plant$pl_g_m_s, test_expected_plant)
  expect_equal(test_results_plant$pl_kg_m_h, test_expected_plant)
  expect_equal(test_results_plant$pl_kg_m_s, test_expected_plant)
  expect_equal(test_results_plant$pl_cm_s, test_expected_plant)
  expect_equal(test_results_plant$pl_cm_h, test_expected_plant)
  expect_equal(test_results_plant$pl_dm_h, test_expected_plant)
  expect_equal(test_results_plant$pl_g_h, test_expected_plant)
  expect_equal(test_results_plant$pl_kg_h, test_expected_plant)
  # sapw
  expect_equal(test_results_sapw$pl_cm_cm_h, test_expected_sapw)
  expect_equal(test_results_sapw$pl_cm_m_s, test_expected_sapw)
  expect_equal(test_results_sapw$pl_dm_dm_h, test_expected_sapw)
  expect_equal(test_results_sapw$pl_dm_dm_s, test_expected_sapw)
  expect_equal(test_results_sapw$pl_mm_mm_s, test_expected_sapw)
  expect_equal(test_results_sapw$pl_g_m_s, test_expected_sapw)
  expect_equal(test_results_sapw$pl_kg_m_h, test_expected_sapw)
  expect_equal(test_results_sapw$pl_kg_m_s, test_expected_sapw)
  expect_equal(test_results_sapw$pl_cm_s, test_expected_sapw)
  expect_equal(test_results_sapw$pl_cm_h, test_expected_sapw)
  expect_equal(test_results_sapw$pl_dm_h, test_expected_sapw)
  expect_equal(test_results_sapw$pl_g_h, test_expected_sapw)
  expect_equal(test_results_sapw$pl_kg_h, test_expected_sapw)
  # leafarea
  expect_equal(test_results_leafarea$pl_cm_cm_h, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_cm_m_s, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_dm_dm_h, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_dm_dm_s, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_mm_mm_s, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_g_m_s, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_kg_m_h, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_kg_m_s, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_cm_s, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_cm_h, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_dm_h, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_g_h, test_expected_leafarea, tolerance = 0.001)
  expect_equal(test_results_leafarea$pl_kg_h, test_expected_leafarea, tolerance = 0.001)
})


test_bad_sapw_md <- data.frame(pl_code = c('pl_cm_cm_h', 'pl_cm_m_s', 'pl_dm_dm_h',
                                       'pl_dm_dm_s', 'pl_mm_mm_s', 'pl_g_m_s',
                                       'pl_kg_m_h', 'pl_kg_m_s', 'pl_cm_s',
                                       'pl_cm_h', 'pl_dm_h', 'pl_g_h', 'pl_kg_h'),
                           pl_sapw_area = rep(200, 13),
                           pl_leaf_area = rep(NA, 13),
                           pl_sap_units = c('“cm3 cm-2 h-1”', '“cm3 m-2 s-1”',
                                            '“dm3 dm-2 h-1”', '“dm3 dm-2 s-1”',
                                            '“mm3 mm-2 s-1”', '“g m-2 s-1”',
                                            '“kg m-2 h-1”', '“kg m-2 s-1”',
                                            '“cm3 s-1”', '“cm3 h-1”', '“dm3 h-1”',
                                            '“g h-1”', '“kg h-1”'))
test_bad2_sapw_md <- data.frame(pl_code = c('pl_cm_cm_h', 'pl_cm_m_s', 'pl_dm_dm_h',
                                           'pl_dm_dm_s', 'pl_mm_mm_s', 'pl_g_m_s',
                                           'pl_kg_m_h', 'pl_kg_m_s', 'pl_cm_s',
                                           'pl_cm_h', 'pl_dm_h', 'pl_g_h', 'pl_kg_h'),
                               pl_sapw_area = rep(NA, 13),
                               pl_leaf_area = rep(14, 13),
                               pl_sap_units = c('“cm3 cm-2 h-1”', '“cm3 m-2 s-1”',
                                                '“dm3 dm-2 h-1”', '“dm3 dm-2 s-1”',
                                                '“mm3 mm-2 s-1”', '“g m-2 s-1”',
                                                '“kg m-2 h-1”', '“kg m-2 s-1”',
                                                '“cm3 s-1”', '“cm3 h-1”', '“dm3 h-1”',
                                                '“g h-1”', '“kg h-1”'))

test_that('conversion fails when there is NAs in leaf area or sapwood', {
  expect_error(qc_sapw_conversion(test_data, test_bad_sapw_md,
                                  output_units = 'leaf'),
               'leaf area values are missing')
  expect_error(qc_sapw_conversion(test_data, test_bad2_sapw_md,
                                  output_units = 'sapwood'),
               'sapwood area values are missing')
})
