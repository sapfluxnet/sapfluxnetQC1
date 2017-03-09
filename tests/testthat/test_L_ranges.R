library(sapfluxnetQC1)

context('L1. Ranges')

# needed data
load('FOO.RData')
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

# sapf_data modifications (we trim the data to lift up computation)
BAR <- FOO[1:5000, , ]
bar_sapf <- get_sapf(BAR)
bar_sapf_flags <- get_sapf_flags(BAR)
bar_sapf_flags[1:10, -1] <- ''
bar_plant_md <- get_plant_md(BAR)

# cm3cm-2h-1
bar_sapf[1:10, 2] <- c(-0.01, -5, -10, -125, -1000,
                       190.001, 215, 1546, 318, 250)
bar_sapf[1:10, 3] <- c(-0.01, -5, -10, -125, -1000,
                       190.001, 215, 1546, 318, 250)
bar_sapf[1:10, 4] <- c(-0.01, -5, -10, -125, -1000,
                       190.001, 215, 1546, 318, 250)

# cm3m-2s-1
bar_cm3m2s1 <- bar_sapf
bar_cm3m2s1[,2] <- bar_cm3m2s1[,2]/0.36
bar_cm3m2s1[,3] <- bar_cm3m2s1[,3]/0.36
bar_cm3m2s1[,4] <- bar_cm3m2s1[,4]/0.36
bar_md_cm3m2s1 <- bar_plant_md
bar_md_cm3m2s1$pl_sap_units <- '“cm3 m-2 s-1”'

# dm3dm2h1
bar_dm3dm2h1 <- bar_sapf
bar_dm3dm2h1[,2] <- bar_dm3dm2h1[,2]/10
bar_dm3dm2h1[,3] <- bar_dm3dm2h1[,3]/10
bar_dm3dm2h1[,4] <- bar_dm3dm2h1[,4]/10
bar_md_dm3dm2h1 <- bar_plant_md
bar_md_dm3dm2h1$pl_sap_units <- '“dm3 dm-2 h-1”'

# dm3dm2s1
bar_dm3dm2s1 <- bar_sapf
bar_dm3dm2s1[,2] <- bar_dm3dm2s1[,2]/36000
bar_dm3dm2s1[,3] <- bar_dm3dm2s1[,3]/36000
bar_dm3dm2s1[,4] <- bar_dm3dm2s1[,4]/36000
bar_md_dm3dm2s1 <- bar_plant_md
bar_md_dm3dm2s1$pl_sap_units <- '“dm3 dm-2 s-1”'

# mm3mm2s1
bar_mm3mm2s1 <- bar_sapf
bar_mm3mm2s1[,2] <- bar_mm3mm2s1[,2]/360
bar_mm3mm2s1[,3] <- bar_mm3mm2s1[,3]/360
bar_mm3mm2s1[,4] <- bar_mm3mm2s1[,4]/360
bar_md_mm3mm2s1 <- bar_plant_md
bar_md_mm3mm2s1$pl_sap_units <- '“mm3 mm-2 s-1”'

# g1m2s1
bar_g1m2s1 <- bar_sapf
bar_g1m2s1[,2] <- bar_g1m2s1[,2]/0.36
bar_g1m2s1[,3] <- bar_g1m2s1[,3]/0.36
bar_g1m2s1[,4] <- bar_g1m2s1[,4]/0.36
bar_md_g1m2s1 <- bar_plant_md
bar_md_g1m2s1$pl_sap_units <- '“g m-2 s-1”'

# kg1m2h1
bar_kg1m2h1 <- bar_sapf
bar_kg1m2h1[,2] <- bar_kg1m2h1[,2]/0.1
bar_kg1m2h1[,3] <- bar_kg1m2h1[,3]/0.1
bar_kg1m2h1[,4] <- bar_kg1m2h1[,4]/0.1
bar_md_kg1m2h1 <- bar_plant_md
bar_md_kg1m2h1$pl_sap_units <- '“kg m-2 h-1”'

# kg1m2s1
bar_kg1m2s1 <- bar_sapf
bar_kg1m2s1[,2] <- bar_kg1m2s1[,2]/360
bar_kg1m2s1[,3] <- bar_kg1m2s1[,3]/360
bar_kg1m2s1[,4] <- bar_kg1m2s1[,4]/360
bar_md_kg1m2s1 <- bar_plant_md
bar_md_kg1m2s1$pl_sap_units <- '“kg m-2 s-1”'

# cm3h1
bar_cm3h1 <- bar_sapf
bar_cm3h1[,2] <- bar_cm3h1[,2] * bar_plant_md$pl_sapw_area[1]
bar_cm3h1[,3] <- bar_cm3h1[,3] * bar_plant_md$pl_sapw_area[2]
bar_cm3h1[,4] <- bar_cm3h1[,4] * bar_plant_md$pl_sapw_area[3]
bar_md_cm3h1 <- bar_plant_md
bar_md_cm3h1$pl_sap_units <- '“cm3 h-1”'

# cm3s1
bar_cm3s1 <- bar_sapf
bar_cm3s1[,2] <- bar_cm3s1[,2] * bar_plant_md$pl_sapw_area[1]/3600
bar_cm3s1[,3] <- bar_cm3s1[,3] * bar_plant_md$pl_sapw_area[2]/3600
bar_cm3s1[,4] <- bar_cm3s1[,4] * bar_plant_md$pl_sapw_area[3]/3600
bar_md_cm3s1 <- bar_plant_md
bar_md_cm3s1$pl_sap_units <- '“cm3 s-1”'

# dm3h1
bar_dm3h1 <- bar_sapf
bar_dm3h1[,2] <- bar_dm3h1[,2] * bar_plant_md$pl_sapw_area[1]/1000
bar_dm3h1[,3] <- bar_dm3h1[,3] * bar_plant_md$pl_sapw_area[2]/1000
bar_dm3h1[,4] <- bar_dm3h1[,4] * bar_plant_md$pl_sapw_area[3]/1000
bar_md_dm3h1 <- bar_plant_md
bar_md_dm3h1$pl_sap_units <- '“dm3 h-1”'

# gh1
bar_gh1 <- bar_sapf
bar_gh1[,2] <- bar_gh1[,2] * bar_plant_md$pl_sapw_area[1]
bar_gh1[,3] <- bar_gh1[,3] * bar_plant_md$pl_sapw_area[2]
bar_gh1[,4] <- bar_gh1[,4] * bar_plant_md$pl_sapw_area[3]
bar_md_gh1 <- bar_plant_md
bar_md_gh1$pl_sap_units <- '“g h-1”'

# kgh1
bar_kgh1 <- bar_sapf
bar_kgh1[,2] <- bar_kgh1[,2] * bar_plant_md$pl_sapw_area[1]/1000
bar_kgh1[,3] <- bar_kgh1[,3] * bar_plant_md$pl_sapw_area[2]/1000
bar_kgh1[,4] <- bar_kgh1[,4] * bar_plant_md$pl_sapw_area[3]/1000
bar_md_kgh1 <- bar_plant_md
bar_md_kgh1$pl_sap_units <- '“kg h-1”'

################################################################################
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

test_that('sapf ranges are checked correctly (sapwood level)', {
  bar_sapf_test <- qc_sapf_ranges(bar_sapf, bar_plant_md, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_sapf_test) == 'RANGE_WARN'), 30)
  expect_equal(sum(as.matrix(bar_sapf_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_sapf_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_sapf_test[,4]) == 'RANGE_WARN'), 10)

  bar_cm3m2s1_test <- qc_sapf_ranges(bar_cm3m2s1, bar_md_cm3m2s1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_cm3m2s1_test) == 'RANGE_WARN'), 30)
  expect_equal(sum(as.matrix(bar_cm3m2s1_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_cm3m2s1_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_cm3m2s1_test[,4]) == 'RANGE_WARN'), 10)

  bar_dm3dm2h1_test <- qc_sapf_ranges(bar_dm3dm2h1, bar_md_dm3dm2h1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_dm3dm2h1_test) == 'RANGE_WARN'), 30)
  expect_equal(sum(as.matrix(bar_dm3dm2h1_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_dm3dm2h1_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_dm3dm2h1_test[,4]) == 'RANGE_WARN'), 10)

  bar_dm3dm2s1_test <- qc_sapf_ranges(bar_dm3dm2s1, bar_md_dm3dm2s1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_dm3dm2s1_test) == 'RANGE_WARN'), 30)
  expect_equal(sum(as.matrix(bar_dm3dm2s1_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_dm3dm2s1_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_dm3dm2s1_test[,4]) == 'RANGE_WARN'), 10)

  bar_mm3mm2s1_test <- qc_sapf_ranges(bar_mm3mm2s1, bar_md_mm3mm2s1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_mm3mm2s1_test) == 'RANGE_WARN'), 30)
  expect_equal(sum(as.matrix(bar_mm3mm2s1_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_mm3mm2s1_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_mm3mm2s1_test[,4]) == 'RANGE_WARN'), 10)

  bar_g1m2s1_test <- qc_sapf_ranges(bar_g1m2s1, bar_md_g1m2s1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_g1m2s1_test) == 'RANGE_WARN'), 30)
  expect_equal(sum(as.matrix(bar_g1m2s1_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_g1m2s1_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_g1m2s1_test[,4]) == 'RANGE_WARN'), 10)

  bar_kg1m2h1_test <- qc_sapf_ranges(bar_kg1m2h1, bar_md_kg1m2h1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_kg1m2h1_test) == 'RANGE_WARN'), 30)
  expect_equal(sum(as.matrix(bar_kg1m2h1_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_kg1m2h1_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_kg1m2h1_test[,4]) == 'RANGE_WARN'), 10)

  bar_kg1m2s1_test <- qc_sapf_ranges(bar_kg1m2s1, bar_md_kg1m2s1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_kg1m2s1_test) == 'RANGE_WARN'), 30)
  expect_equal(sum(as.matrix(bar_kg1m2s1_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_kg1m2s1_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(bar_kg1m2s1_test[,4]) == 'RANGE_WARN'), 10)
})

test_that('sapf ranges are checked correctly (tree level)', {
  bar_cm3h1_test <- qc_sapf_ranges(bar_cm3h1, bar_md_cm3h1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_cm3h1_test) == 'RANGE_WARN'), 22)
  expect_equal(sum(as.matrix(bar_cm3h1_test[,2]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_cm3h1_test[,3]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_cm3h1_test[,4]) == 'RANGE_WARN'), 10)

  bar_cm3s1_test <- qc_sapf_ranges(bar_cm3s1, bar_md_cm3s1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_cm3s1_test) == 'RANGE_WARN'), 22)
  expect_equal(sum(as.matrix(bar_cm3s1_test[,2]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_cm3s1_test[,3]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_cm3s1_test[,4]) == 'RANGE_WARN'), 10)

  bar_dm3h1_test <- qc_sapf_ranges(bar_dm3h1, bar_md_dm3h1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_dm3h1_test) == 'RANGE_WARN'), 22)
  expect_equal(sum(as.matrix(bar_dm3h1_test[,2]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_dm3h1_test[,3]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_dm3h1_test[,4]) == 'RANGE_WARN'), 10)

  bar_gh1_test <- qc_sapf_ranges(bar_gh1, bar_md_gh1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_gh1_test) == 'RANGE_WARN'), 22)
  expect_equal(sum(as.matrix(bar_gh1_test[,2]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_gh1_test[,3]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_gh1_test[,4]) == 'RANGE_WARN'), 10)

  bar_kgh1_test <- qc_sapf_ranges(bar_kgh1, bar_md_kgh1, bar_sapf_flags)

  expect_equal(sum(as.matrix(bar_kgh1_test) == 'RANGE_WARN'), 22)
  expect_equal(sum(as.matrix(bar_kgh1_test[,2]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_kgh1_test[,3]) == 'RANGE_WARN'), 6)
  expect_equal(sum(as.matrix(bar_kgh1_test[,4]) == 'RANGE_WARN'), 10)
})
