library(sapfluxnetQC1)

###################################################
# logging                                         #
log_sapfluxnet_setup('test.log', 'test', "DEBUG") #
###################################################

context('A1. Maps argument checks')

test_that('arguments error are correct', {

  foo_data <- data.frame(x = rnorm(5,1,1),
                         si_country = c(rep('IRE', 2), rep('IRL', 3)))

  foo_wrong_data <- data.frame(x = rnorm(5,1,1),
                               y = rpois(5,1))

  expect_error(qc_download_maps(foo_wrong_data, getwd()), 'There is no country')
  expect_error(qc_download_maps(c(1,2,3), getwd()), 'object is not a data.frame')
  expect_error(qc_download_maps(foo_data, 'foo_folder'),
                 'Destination folder does not exist.')
})

context('A2. Maps Download')

test_that('download works', {

  foo_data <- data.frame(x = rnorm(5,1,1),
                         si_country = c(rep('IRE', 2), rep('IRL', 3)))

  expect_message(qc_download_maps(foo_data), '1 new maps downloaded')
  file.remove('./gadm/gadm41_IRL_0_pk.rds')

  expect_message(qc_download_maps(foo_data), 'not downloaded')
  expect_false(file_test("-f", './gadm/gadm41_IRE_0_pk.rds'))
  expect_true(file_test("-f", './gadm/gadm41_IRL_0_pk.rds'))
  file.remove('./gadm/gadm41_IRL_0_pk.rds')
})
