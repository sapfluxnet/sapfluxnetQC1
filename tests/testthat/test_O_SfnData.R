library(sapfluxnetQC1)

################################################################################
context('O1. SfnData Class')

# load foo objects
load('foo_objects.RData')

# build the SfnData object with foo_data
foo_sfndata <- sfn_data_constructor(
  sapf_data = sapf_data_fixed,
  env_data = env_data_fixed,
  site_md = site_md_coordfix,
  stand_md = stand_md,
  species_md = species_md_spnames,
  plant_md = plant_md_spnames,
  env_md = env_md,
  solar_timestamp = NULL
)

rows_joined <- length(
  dplyr::full_join(tibble::as_tibble(sapf_data_fixed)[,1],
                   tibble::as_tibble(env_data_fixed)[,1])[[1]]
)

test_that('SfnData class works', {
  expect_is(foo_sfndata, "SfnData")
  expect_error(
    sfn_data_constructor(
      sapf_data = c(1,2,3),
      env_data = env_data_fixed,
      site_md = site_md_coordfix,
      stand_md = stand_md,
      species_md = species_md_spnames,
      plant_md = plant_md_spnames,
      env_md = env_md
    ),
    'Data and/or metadata objects provided are not data.frames')
  expect_error(
    sfn_data_constructor(
      sapf_data = sapf_data_fixed,
      env_data = c(1,2,3),
      site_md = site_md_coordfix,
      stand_md = stand_md,
      species_md = species_md_spnames,
      plant_md = plant_md_spnames,
      env_md = env_md
    ),
  'Data and/or metadata objects provided are not data.frames')
  expect_error(
    sfn_data_constructor(
      sapf_data = sapf_data_fixed,
      env_data = env_data_fixed,
      site_md = c(1,2,3),
      stand_md = stand_md,
      species_md = species_md_spnames,
      plant_md = plant_md_spnames,
      env_md = env_md
    ),
    'Data and/or metadata objects provided are not data.frames')
  expect_error(
    sfn_data_constructor(
      sapf_data = sapf_data_fixed,
      env_data = env_data_fixed,
      site_md = site_md_coordfix,
      stand_md = c(1,2,3),
      species_md = species_md_spnames,
      plant_md = plant_md_spnames,
      env_md = env_md
    ),
    'Data and/or metadata objects provided are not data.frames')
  expect_error(
    sfn_data_constructor(
      sapf_data = sapf_data_fixed,
      env_data = env_data_fixed,
      site_md = site_md_coordfix,
      stand_md = stand_md,
      species_md = c(1,2,3),
      plant_md = plant_md_spnames,
      env_md = env_md
    ),
    'Data and/or metadata objects provided are not data.frames')
  expect_error(
    sfn_data_constructor(
      sapf_data = sapf_data_fixed,
      env_data = env_data_fixed,
      site_md = site_md_coordfix,
      stand_md = stand_md,
      species_md = species_md_spnames,
      plant_md = c(1,2,3),
      env_md = env_md
    ),
    'Data and/or metadata objects provided are not data.frames')
  expect_error(
    sfn_data_constructor(
      sapf_data = sapf_data_fixed,
      env_data = env_data_fixed,
      site_md = site_md_coordfix,
      stand_md = stand_md,
      species_md = species_md_spnames,
      plant_md = plant_md_spnames,
      env_md = c(1,2,3)
    ),
    'Data and/or metadata objects provided are not data.frames')
})

# test_that('show method works', {
#   expect_output(show(foo_sfndata), 'SfnData object')
#   expect_output(show(foo_sfndata), 'Sapflow data: 2310 observations of 5 trees/plants')
#   expect_output(show(foo_sfndata), 'Environmental data: 2310 observations.')
#   expect_output(show(foo_sfndata),
#                 'TIMESTAMP span, from 2011-06-23 21:59:59 to 2011-11-24 05:00:00')
#   expect_output(show(foo_sfndata), 'Present FLAGS in data:  NA_PRESENT ')
# })

test_that('subset method works', {
  foo_subset <- foo_sfndata[1:2000, 1:3, 1:2]

  expect_equal(nrow(get_sapf(foo_subset)), 2000)
  expect_equal(nrow(get_env(foo_subset)), 2000)
  expect_equal(length(get_timestamp(foo_subset)), 2000)
  expect_equal(length(get_solar_timestamp(foo_subset)), 2000)
  expect_equal(length(get_si_code(foo_subset)), 2000)
  expect_equal(nrow(get_sapf_flags(foo_subset)), 2000)
  expect_equal(nrow(get_env_flags(foo_subset)), 2000)
  expect_equal(ncol(get_sapf(foo_subset)), 4)
  expect_equal(ncol(get_env(foo_subset)), 3)
  expect_equal(ncol(get_sapf_flags(foo_subset)), 4)
  expect_equal(ncol(get_env_flags(foo_subset)), 3)
  expect_identical(get_site_md(foo_subset), get_site_md(foo_sfndata))
  expect_identical(get_stand_md(foo_subset), get_stand_md(foo_sfndata))
  expect_identical(get_species_md(foo_subset), get_species_md(foo_sfndata))
  expect_identical(get_plant_md(foo_subset), get_plant_md(foo_sfndata))
  expect_identical(get_env_md(foo_subset), get_env_md(foo_sfndata))
})

test_that('get methods work', {
  expect_is(get_sapf(foo_sfndata), 'data.frame')
  expect_equal(ncol(get_sapf(foo_sfndata)), ncol(sapf_data_fixed))
  expect_equal(nrow(get_sapf(foo_sfndata)), rows_joined)
  expect_is(get_env(foo_sfndata), 'data.frame')
  expect_equal(ncol(get_env(foo_sfndata)), ncol(env_data_fixed))
  expect_equal(nrow(get_env(foo_sfndata)), rows_joined)
  expect_is(get_sapf_flags(foo_sfndata), 'data.frame')
  expect_equal(ncol(get_sapf_flags(foo_sfndata)), ncol(sapf_data_fixed))
  expect_equal(nrow(get_sapf_flags(foo_sfndata)), rows_joined)
  expect_is(get_env_flags(foo_sfndata), 'data.frame')
  expect_equal(ncol(get_env_flags(foo_sfndata)), ncol(env_data_fixed))
  expect_equal(nrow(get_env_flags(foo_sfndata)), rows_joined)
  expect_equal(length(get_si_code(foo_sfndata)), rows_joined)
  expect_equal(length(get_timestamp(foo_sfndata)), rows_joined)
  expect_equal(length(get_solar_timestamp(foo_sfndata)), rows_joined)
  expect_is(get_site_md(foo_sfndata), 'data.frame')
  expect_is(get_stand_md(foo_sfndata), 'data.frame')
  expect_is(get_species_md(foo_sfndata), 'data.frame')
  expect_is(get_plant_md(foo_sfndata), 'data.frame')
  expect_is(get_env_md(foo_sfndata), 'data.frame')
})

test_that('assignation method works', {
  foo_sapf <- get_sapf(foo_sfndata)
  foo_env <- get_env(foo_sfndata)
  foo_sapf[,-1] <- foo_sapf[,-1]*5
  foo_env[,-1] <- foo_env[,-1]*5
  bar_sapf <- foo_sapf[-(1:5), -1]
  bar_env <- foo_env[-(1:5), -1]

  expect_error((get_sapf(foo_sfndata) <- bar_sapf),
               'new data is not valid')
  expect_error((get_sapf(foo_sfndata) <- 1),
               'assignment of an object of class “numeric” is not valid')
  expect_error((get_sapf(foo_sfndata) <- '1'),
               'assignment of an object of class “character” is not valid')
  expect_error((get_env(foo_sfndata) <- bar_env),
               'new data is not valid')
  expect_error((get_env(foo_sfndata) <- 1),
               'assignment of an object of class “numeric” is not valid')
  expect_error((get_env(foo_sfndata) <- '1'),
               'assignment of an object of class “character” is not valid')
  expect_error((get_sapf_flags(foo_sfndata) <- bar_sapf),
               'new data is not valid')
  expect_error((get_sapf_flags(foo_sfndata) <- 1),
               'assignment of an object of class “numeric” is not valid')
  expect_error((get_sapf_flags(foo_sfndata) <- '1'),
               'assignment of an object of class “character” is not valid')
  expect_error((get_env_flags(foo_sfndata) <- bar_env),
               'new data is not valid')
  expect_error((get_env_flags(foo_sfndata) <- 1),
               'assignment of an object of class “numeric” is not valid')
  expect_error((get_env_flags(foo_sfndata) <- '1'),
               'assignment of an object of class “character” is not valid')
  expect_error((get_si_code(foo_sfndata) <- 'FOO_FOO_FOO'),
               'dimensions are incorrect')
  expect_error((get_si_code(foo_sfndata) <- 1),
               'assignment of an object of class “numeric” is not valid')
  expect_error((get_timestamp(foo_sfndata) <- '2011-06-23 21:59:59 -03'),
               'assignment of an object of class “character” is not valid')
  expect_error((get_timestamp(foo_sfndata) <- 1),
               'assignment of an object of class “numeric” is not valid')
  expect_error((get_timestamp(foo_sfndata) <- as.POSIXct('2011-06-23 21:59:59 -03')),
               'dimensions are incorrect')
  expect_error((get_solar_timestamp(foo_sfndata) <- '2011-06-23 21:59:59 -03'),
               'assignment of an object of class “character” is not valid')
  expect_error((get_solar_timestamp(foo_sfndata) <- 1),
               'assignment of an object of class “numeric” is not valid')
  expect_error((get_solar_timestamp(foo_sfndata) <- as.POSIXct('2011-06-23 21:59:59 -03')),
               'dimensions are incorrect')
})
