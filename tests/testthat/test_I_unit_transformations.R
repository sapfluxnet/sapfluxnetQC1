library(sapfluxnetQC1)

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

################################################################################
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

################################################################################
context('I3. Sapflow unit conversion')

test_that('argument checks works', {
  expect_error(qc_sapw_conversion('not a data frame', qc_get_sapw_md(pl_data), 'plant'),
               'data and/or sapw_md objects are not data frames')
  expect_error(qc_sapw_conversion(dl_data('foo.xlsx', 'sapflow_hd'),
                                  'not a data frame', 'plant'),
               'data and/or sapw_md objects are not data frames')
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

################################################################################
context('I4. Radiation unit conversion')

env_hd <- suppressWarnings(suppressMessages(dl_data('foo_env.csv','environmental_hd')))
ppfd_in <- LakeMetabolizer::sw.to.par.base(env_hd$sw_in)

test_that('argument checks work', {
  expect_error(qc_rad_conversion('not a data frame'),
               'data object is not a data frame')
})

test_that('function works', {
  expect_message(
    qc_rad_conversion(cbind(env_hd, ppfd_in)),
    'Radiation in both sw_in and ppfd_in units already exists.'
  )
  expect_warning(
    qc_rad_conversion(subset(env_hd, select = -sw_in),'ppfd_in'),
    'Both sw_in and ppfd_in are missing.'
  )
})

# test data frames
test_data_sw_in <- data.frame(TIMESTAMP = c(1, 2, 3, 4, 5, 6, 7),
                        sw_in = c(0.1, 1, 2, 5, 10, 100, 1000))

test_data_ppfd_in <- data.frame(TIMESTAMP = c(1, 2, 3, 4, 5, 6, 7),
                         ppfd_in = c(0.1, 1, 2, 5, 10, 100, 1000))

# expected results
test_results_expected_ppfd_in <- c(0.2114, 2.114, 4.228, 10.57, 21.14, 211.4, 2114)
test_results_expected_sw_in <- c(0.0473, 0.473, 0.946, 2.365, 4.73, 47.3, 473)

# results applying the function
test_results_ppfd_in <- round(qc_rad_conversion(test_data_sw_in), 4)
test_results_sw_in <- round(qc_rad_conversion(test_data_ppfd_in), 4)

test_that('conversion is made correctly', {
  expect_equal(test_results_ppfd_in$ppfd_in, test_results_expected_ppfd_in)
  expect_equal(test_results_sw_in$sw_in, test_results_expected_sw_in)
})

test_that('the new variable is added to the table and is numeric', {
  expect_equal(
    c(names(env_hd),'ppfd_in'),
    names(qc_rad_conversion(env_hd))
  )
  expect_is(qc_rad_conversion(env_hd)$ppfd_in, 'numeric')
})

################################################################################
context('I5. VPD calculation')

load('FOO.RData')
foo_env <- get_env(FOO)
orig_vpd <- foo_env[['vpd']]
foo_no_ta <- foo_env[,c('TIMESTAMP', 'rh', 'vpd')]
foo_no_rh <- foo_env[,c('TIMESTAMP', 'ta', 'vpd')]
foo_no_vpd <- foo_env[,c('TIMESTAMP', 'rh', 'ta')]

test_that('argument checks work', {
  expect_error(qc_vpd('tururu'), 'data object is not a data frame')
  expect_error(qc_vpd(foo_no_ta), 'data not contains rh and/or ta variables')
  expect_error(qc_vpd(foo_no_rh), 'data not contains rh and/or ta variables')
  expect_error(qc_vpd(foo_env), 'data already has a vpd variable')
})

test_that('function returns values correctly', {
  res <- qc_vpd(foo_no_vpd)

  expect_is(res, 'data.frame')
  expect_false(is.null(res[['vpd']]))
  # original data is wrong, with a less order of magnitude
  expect_equal(res[['vpd']], orig_vpd*10, tolerance = 0.05)
})


################################################################################
context('I6. Soil texture classification')

test_data <- data.frame(st_soil_texture = 'LOAM', st_clay_perc = 12,
                        st_silt_perc = 54, st_sand_perc = 34)

test_that('Errors are raised correctly', {
  expect_error(qc_soil_texture('test_data'), 'Data is not a data frame')
  expect_error(qc_soil_texture(test_data[, -1]),
               'At least one of the required variables is missing in data')
  expect_error(qc_soil_texture(test_data[, -3]),
               'At least one of the required variables is missing in data')
  expect_error(qc_soil_texture(test_data[, - c(2,3)]),
               'At least one of the required variables is missing in data')
  expect_error(qc_soil_texture(test_data[, - c(1,3)]),
               'At least one of the required variables is missing in data')
  expect_error(qc_soil_texture(test_data[, - c(2,4)]),
               'At least one of the required variables is missing in data')
  expect_error(qc_soil_texture(as.data.frame(test_data[, - c(1,2,4)])),
               'At least one of the required variables is missing in data')
  expect_error(qc_soil_texture(as.data.frame(test_data[, - c(1,2,3)])),
               'At least one of the required variables is missing in data')
})

test_data_sum <- data.frame(st_soil_texture = 'LOAM', st_clay_perc = 12,
                            st_silt_perc = 40, st_sand_perc = 34)
suppressWarnings(test_data_res <- qc_soil_texture(test_data_sum))

test_that('Error if sum different from 100', {
  expect_warning(qc_soil_texture(test_data_sum),
                 'The sum of the different percentages of clay, silt and sand is not equal to 100%')
  # expect_false(suppressWarnings(qc_soil_texture(test_data_sum)))
  expect_identical(test_data_sum, test_data_res)
})

test_data_NA <- data.frame(st_soil_texture = 'LOAM', st_clay_perc = NA,
                           st_silt_perc = 40, st_sand_perc = NA)
suppressMessages(test_data_res <- qc_soil_texture(test_data_NA))

test_that('If NA value for percentage, returns st_soil_texture if not NA', {
  expect_equal(tolower(as.character(test_data_res$st_soil_texture)), test_data_res$st_USDA_soil_texture)
  expect_message(qc_soil_texture(test_data_NA), 'One or more percentages are missing.')
})

test_data_NA_NA <- data.frame(st_soil_texture = NA, st_clay_perc = NA,
                              st_silt_perc = 40, st_sand_perc = 34)
suppressWarnings(test_data_res <- qc_soil_texture(test_data_NA_NA))

test_that('If NA value for percentage AND soil texture', {
  expect_warning(qc_soil_texture(test_data_NA_NA),
                 'There is no information about the soil texture')
  # expect_false(suppressWarnings(qc_soil_texture(test_data_NA_NA)))
  expect_identical(test_data_NA_NA, test_data_res)
})

test_data_dec <- data.frame(st_soil_texture = NA, st_clay_perc = 0.51,
                            st_silt_perc = 0.36, st_sand_perc = 0.13)
test_data_res_dec <- qc_soil_texture(test_data_dec)

test_data_perc <- data.frame(st_soil_texture = NA, st_clay_perc = 51,
                             st_silt_perc = 36, st_sand_perc = 13)
test_data_res_perc <- qc_soil_texture(test_data_perc)

test_that('If value in decimal, function converts it to % and the result is the same', {
  expect_equal(test_data_res_dec$st_USDA_soil_texture, test_data_res_perc$st_USDA_soil_texture)
})

test_data_dif <- data.frame(st_soil_texture = 'LOAM', st_clay_perc = 12,
                            st_silt_perc = 54, st_sand_perc = 34)

test_that('If returned value differs from original category, it gives a warning', {
  expect_warning(qc_soil_texture(test_data_dif), 'Calculated soil texture class differs')
})

# Clay
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 55.5,
                                st_silt_perc = 10, st_sand_perc = 34.5)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (clay)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'clay')
})

# Silty clay
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 50,
                                st_silt_perc = 45, st_sand_perc = 5)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (silty clay)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'silty clay')
})

# Sandy clay
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 40,
                                st_silt_perc = 10, st_sand_perc = 50)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (sandy clay)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'sandy clay')
})

# Clay loam
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 30,
                                st_silt_perc = 30, st_sand_perc = 40)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (clay loam)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'clay loam')
})

# Silty clay loam
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 30,
                                st_silt_perc = 60, st_sand_perc = 10)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (silty clay loam)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'silty clay loam')
})

# Sandy clay loam
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 30,
                                st_silt_perc = 20, st_sand_perc = 50)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (sandy clay loam)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'sandy clay loam')
})

# Loam
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 20,
                                st_silt_perc = 40, st_sand_perc = 40)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (loam)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'loam')
})

# Silty loam
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 20,
                                st_silt_perc = 60, st_sand_perc = 20)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (silty loam)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'silty loam')
})


# Sandy loam
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 10,
                                st_silt_perc = 20, st_sand_perc = 70)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (sandy loam)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'sandy loam')
})

# Silt
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 10,
                                st_silt_perc = 85, st_sand_perc = 5)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (silt)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'silt')
})

# Loamy sand
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 5,
                                st_silt_perc = 15, st_sand_perc = 80)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct (loamy sand)', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'loamy sand')
})

# Sand
test_data_texture <- data.frame(st_soil_texture = NA, st_clay_perc = 5,
                                st_silt_perc = 5, st_sand_perc = 90)
test_data_res <- qc_soil_texture(test_data_texture)

test_that('Category chosen by the function is correct', {
  expect_equivalent(test_data_res$st_USDA_soil_texture, 'sand')
})

## test that results not produces lists as columns

test_that('st_USDA_soil_texture is not a list', {
  expect_is(test_data_res$st_USDA_soil_texture, 'character')
  expect_false(is.list(test_data_res$st_USDA_soil_texture))
})

## test that when several classes are returned the first one is returned
test_several <- data.frame(st_soil_texture = 'LOAM', st_clay_perc = 20,
                           st_silt_perc = 20, st_sand_perc = 60)
test_several_res <- suppressWarnings(qc_soil_texture(test_several))

test_that('st_USDA_soil_texture is correctly generated when several classes', {
  expect_warning(qc_soil_texture(test_several),
                 'Calculated soil texture class differs ')
  expect_identical(test_several_res$st_USDA_soil_texture,
                   'sandy clay loam')
})

################################################################################
context('I7. qc_transformation_vars')

test_that('results are ok', {
  load('FOO.RData')
  transf_vars_info <- qc_transformation_vars(FOO)

  expect_is(transf_vars_info, 'data.frame')
  expect_false(transf_vars_info$Presence[1])
  expect_false(transf_vars_info$Presence[11])
  expect_equal(sum(transf_vars_info$Presence), 9)

  get_plant_md(FOO)$pl_sap_units <- NA
  transf_vars_info <- qc_transformation_vars(FOO)

  expect_is(transf_vars_info, 'data.frame')
  expect_false(transf_vars_info$Presence[1])
  expect_false(transf_vars_info$Presence[9])
  expect_false(transf_vars_info$Presence[11])
  expect_equal(sum(transf_vars_info$Presence), 8)

  get_plant_md(FOO)$pl_sapw_area <- c(77.06, NA, 391.30)
  transf_vars_info <- qc_transformation_vars(FOO)

  expect_is(transf_vars_info, 'data.frame')
  expect_false(transf_vars_info$Presence[1])
  expect_false(transf_vars_info$Presence[9])
  expect_false(transf_vars_info$Presence[11])
  expect_true(transf_vars_info$Presence[10])
  expect_equal(sum(transf_vars_info$Presence), 8)

  expect_true(all(
    transf_vars_info$Transformation %in% c('radiation_conversion', 'solar_time',
                                           'vpd_calc', 'sapf_units')
  ))
})

################################################################################
context('I8. qc_transf_list')

load('FOO.RData')
transf_vars_info <- qc_transformation_vars(FOO)
transf_list <- qc_transf_list(transf_vars_info)

test_that('results are correct', {
  expect_is(transf_list, 'data.frame')
  expect_false(transf_list$Available[3])
  expect_false(transf_list$Available[6])
  expect_equal(sum(transf_list$Available), 4)

  expect_true(all(
    transf_list$Transformation %in% c('radiation_conversion', 'solar_time',
                                      'VPD_calculation', 'sapf_units_to_plant',
                                      'sapf_units_to_sapwood',
                                      'sapf_units_to_leaf_area')
  ))
})
