library(sapfluxnetQC1)

context('L1. Outliers')

# needed data
load('FOO.RData')
sapf_data <- get_sapf(FOO)
env_data <- get_env(FOO)
sapf_flags <- get_sapf_flags(FOO)
env_flags <- get_env_flags(FOO)

# data structure & *_to_remove files
df_folder_structure()
dir.create(file.path('Data', 'FOO', 'Lvl_2'), recursive = TRUE)
df_lvl2_folder_structure('FOO')
# file.copy(from = c('FOO.RData'),
#           to = file.path('Data', 'FOO', 'Lvl_2', 'lvl_2_out_warn', 'FOO.RData'))
df_start_status('FOO')
df_set_status(
  'FOO',
  QC = list(DATE = as.character(Sys.Date()), DONE = "TRUE"),
  LVL1 = list(DATE = as.character(Sys.Date()), STORED = "TRUE", TO_LVL2 = 'DONE'),
  LVL2 = list(DATE = as.character(Sys.Date()), STORED = "TRUE", STEP = "WARN",
              TO_REM = "READY", TO_UNITS = "FREEZE")
)

out_to_remove <- data.frame(
  stringsAsFactors = FALSE,
  variable = c(rep(c("AUS_ELL_HB_Edi_Js_1", "AUS_ELL_HB_Era_Js_2", "AUS_ELL_HB_Edi_Js_3"), each = 5),
               rep(c('ta', 'rh', 'vpd', 'ppfd_in', 'ws', 'precip'), each = 5)),
  index = rep(20001:20005, 9)
)

suppressWarnings(write.table(
  out_to_remove,
  file = file.path('Data', 'FOO', 'Lvl_2', 'lvl_2_out_warn', 'FOO_out_to_remove.txt'),
  append = TRUE, row.names = FALSE, col.names = TRUE
))

ranges_to_remove <- data.frame(
  stringsAsFactors = FALSE,
  variable = c(rep(c("AUS_ELL_HB_Edi_Js_1", "AUS_ELL_HB_Era_Js_2", "AUS_ELL_HB_Edi_Js_3"), each = 5),
               rep(c('ta', 'rh', 'vpd', 'ppfd_in', 'ws', 'precip'), each = 5)),
  index = rep(20006:20010, 9)
)

suppressWarnings(write.table(
  ranges_to_remove,
  file = file.path('Data', 'FOO', 'Lvl_2', 'lvl_2_out_warn', 'FOO_ranges_to_remove.txt'),
  append = TRUE, row.names = FALSE, col.names = TRUE
))

# create fake flags
sapf_flags[20001:20010, -1] <- rep(c('OUT_WARN', 'OUT_RANGE'), each = 5)
env_flags[20001:20010, -1] <- rep(c('OUT_WARN', 'OUT_RANGE'), each = 5)

# create fake data for out_remove
sapf_data[20001:20005, -1] <- 100000
env_data[20001:20005, -1] <- 100000

get_env_flags(FOO) <- env_flags[,-1]
get_sapf_flags(FOO) <- sapf_flags[,-1]
get_si_code(FOO) <- rep('FOO', nrow(env_flags))
get_env(FOO) <- env_data[,-1]
get_sapf(FOO) <- sapf_data[,-1]

df_write_SfnData(FOO, 'out_warn')

# remove the outs (TIME CONSUMING!!)
res <- qc_outliers_process('FOO')

# actual tests
test_that('result is an SfnData object', {
  expect_is(res, 'SfnData')
})

test_that('fake flags values have changed correctly', {
  env_flags_res <- get_env_flags(res)
  sapf_flags_res <- get_sapf_flags(res)

  expect_true(all(stringr::str_detect(env_flags_res[20001:20005, -1], 'OUT_REPLACED')))
  expect_true(all(stringr::str_detect(sapf_flags_res[20001:20005, -1], 'OUT_REPLACED')))
  expect_true(all(stringr::str_detect(env_flags_res[20006:20010, -1], 'RANGE_REMOVE')))
  expect_true(all(stringr::str_detect(sapf_flags_res[20006:20010, -1], 'RANGE_REMOVE')))
  expect_false(any(stringr::str_detect(env_flags_res[-c(20001:20005), -1], 'OUT_REPLACED')))
  expect_false(any(stringr::str_detect(env_flags_res[-c(20006:20010), -1], 'RANGE_REMOVE')))
  expect_false(any(stringr::str_detect(sapf_flags_res[-c(20001:20005), -1], 'OUT_REPLACED')))
  expect_false(any(stringr::str_detect(sapf_flags_res[-c(20006:20010), -1], 'RANGE_REMOVE')))
})

test_that('values are substituted', {
  env_data_res <- get_env(res)
  sapf_data_res <- get_sapf(res)

  expect_true(all(is.na(env_data_res[20006:20010, -1])))
  expect_true(all(is.na(sapf_data_res[20006:20010, -1])))
  expect_false(any(env_data_res[20001:20005, -1] == env_data[20001:20005, -1]))
  expect_false(any(sapf_data_res[20001:20005, -1] == sapf_data[20001:20005, -1]))
})

################################################################################
context('L2. Data flow inside level 2')

df_warn_to_rem()

test_that('files are written correctly', {
  expect_true(
    file.exists(file.path('Data', 'FOO', 'Lvl_2', 'lvl_2_out_rem', 'FOO.RData'))
  )

  env_data_pre <- get_env(df_read_SfnData('FOO', 'out_warn'))[20001:20010, -1]
  env_data_post <- get_env(df_read_SfnData('FOO', 'out_rem'))[20001:20010, -1]
  sapf_data_pre <- get_sapf(df_read_SfnData('FOO', 'out_warn'))[20001:20010, -1]
  sapf_data_post <- get_sapf(df_read_SfnData('FOO', 'out_rem'))[20001:20010, -1]

  expect_true(all(is.na(env_data_post[6:10,])))
  expect_true(all(is.na(sapf_data_post[6:10,])))
  expect_false(all(is.na(env_data_pre[6:10,])))
  expect_false(all(is.na(sapf_data_pre[6:10,])))
  expect_false(any(env_data_pre[1:5,] == env_data_post[1:5,]))
  expect_false(any(sapf_data_pre[1:5,] == sapf_data_post[1:5,]))
})

test_that('status file is updated', {
  status_foo <- df_get_status('FOO')

  expect_identical(status_foo$LVL2$TO_REM, 'DONE')
  expect_identical(status_foo$LVL2$TO_UNITS, 'FREEZE')
  expect_identical(status_foo$LVL2$STEP, 'REM')
})




################################################################################
# clean
unlink('Data', recursive = TRUE)
unlink('Logs', recursive = TRUE)
unlink('Reports', recursive = TRUE)
unlink('Templates', recursive = TRUE)

################################################################################
context('L3. Ranges')

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

foo_env$swc_shallow <- c(-0.1, -1, -10, -100, -5,
                         1.1, 11, 111, 59, 65,
                         rnorm(47606, 0.5, 0.01))

foo_env$swc_deep <- c(-0.1, -1, -10, -100, -5,
                         1.1, 11, 111, 59, 65,
                         rnorm(47606, 0.5, 0.01))

# env_flags modified to add the variables not listed at first
foo_env_flags$sw_in <- ''
foo_env_flags$netrad <- ''
foo_env_flags$swc_shallow <- ''
foo_env_flags$swc_deep <- ''

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
  expect_length(qc_range_dic(), 12)
  expect_identical(names(qc_range_dic()),
                   c('ta', 'rh', 'vpd', 'sw_in', 'ppfd_in', 'netrad',
                     'ws', 'precip', 'swc_shallow', 'swc_deep', 'sapf_sapw', 'sapf_tree'))
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
  expect_length(qc_range_dic()[[11]], 2)
  expect_length(qc_range_dic()[[12]], 2)
})

test_that('env ranges are checked correctly', {
  foo_env_test <- qc_env_ranges(foo_env, foo_env_flags)

  expect_equal(sum(as.matrix(foo_env_test) == 'RANGE_WARN'), 100)
  expect_equal(sum(as.matrix(foo_env_test[,2]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,3]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,4]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,5]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,6]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,7]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,8]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,9]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,10]) == 'RANGE_WARN'), 10)
  expect_equal(sum(as.matrix(foo_env_test[,11]) == 'RANGE_WARN'), 10)
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

test_that('qc_out_of_range wrapper function works', {
  BAZ <- qc_out_of_range(BAR)

  expect_is(BAZ, 'SfnData')
})
