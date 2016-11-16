library(sapfluxnetQC1)

context('F1. Dictionaries')

test_that('dictionary functions create the correct class of object', {
  expect_is(create_dic('site_md'), 'list')
  expect_is(create_dic('stand_md'), 'list')
  expect_is(create_dic('species_md'), 'list')
  expect_is(create_dic('plant_md'), 'list')
  expect_is(create_dic('environmental_md'), 'list')
  expect_is(suppressMessages(qc_site_dics('si_igbp')), 'character')
  expect_is(qc_stand_dics('st_terrain'), 'character')
  expect_is(qc_species_dics('sp_leaf_habit'), 'character')
  expect_is(qc_plant_dics('pl_sap_units'), 'character')
  expect_is(qc_env_dics('env_ta'), 'character')
})

test_that('errors in arguments trigger the correct message', {
  expect_error(create_dic('not_A_valid_md'),
               'Provided dicitonary name is not a character or is not a valid name.')
  expect_error(suppressMessages(qc_site_dics('some_other_variable')),
               'some_other_variable')
  expect_error(qc_stand_dics('some_other_variable'), 'some_other_variable')
  expect_error(qc_species_dics('some_other_variable'), 'some_other_variable')
  expect_error(qc_plant_dics('some_other_variable'), 'some_other_variable')
  expect_error(qc_env_dics('some_other_variable'), 'some_other_variable')
})
