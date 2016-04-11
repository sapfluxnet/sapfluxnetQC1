library(sapfluxnetr)

context('G1. Metadata columns check')

file_name <- 'foo.xlsx'
site_md <- suppressMessages(dl_metadata(file_name, 'site_md'))
stand_md <- dl_metadata(file_name, 'stand_md', si_code_loc = site_md)
species_md <- dl_metadata(file_name, 'species_md', si_code_loc = site_md)
plant_md <- dl_metadata(file_name, 'plant_md', si_code_loc = site_md)
env_md <- dl_metadata(file_name, 'environmental_md', si_code_loc = site_md)

test_that('errors are raised correctly', {
  expect_error(qc_md_cols(c('a','b','c'), 'site_md'),
               'Metadata object is not a data frame')
  expect_error(qc_md_cols(site_md, 'not a valid dic'),
               'Provided dictionary name is not')
})

test_that('results are presented in a data frame', {
  expect_is(qc_md_cols(stand_md, 'stand_md'), 'data.frame')
})

test_that('results dimensions are correct', {
  expect_equal(length(qc_md_cols(stand_md, 'stand_md')), 5)
  expect_equal(length(qc_md_cols(site_md, 'site_md')), 5)
  expect_equal(length(qc_md_cols(species_md, 'species_md')), 5)
  expect_equal(length(qc_md_cols(plant_md, 'plant_md')), 5)
  expect_equal(length(qc_md_cols(env_md, 'environmental_md')), 5)
  expect_equal(length(qc_md_cols(stand_md, 'stand_md')[,1]), 17)
  expect_equal(length(qc_md_cols(site_md, 'site_md')[,1]), 20)
  expect_equal(length(qc_md_cols(species_md, 'species_md')[,1]), 5)
  expect_equal(length(qc_md_cols(plant_md, 'plant_md')[,1]), 25)
  expect_equal(length(qc_md_cols(env_md, 'environmental_md')[,1]), 17)
})

site_check <- qc_md_cols(site_md, 'site_md')
site_md_mod <- site_md
site_md_mod$si_remarks <- 'Foo text'
site_mod_check <- qc_md_cols(site_md_mod, 'site_md')
site_md_mod_2 <- site_md
site_md_mod_2$si_name <- 4
site_mod_2_check <- qc_md_cols(site_md_mod_2, 'site_md')

test_that('results are correct', {
  expect_true(all(site_check$PresenceOK))
  expect_false(all(site_check$ClassOK))
  expect_true(all(site_mod_check$ClassOK))
  expect_false(site_mod_2_check$ClassOK[1])
})

context('G2. Factor values checks')

factor_res <- qc_factor_values(site_md, stand_md, species_md, plant_md, env_md)

test_that('results are presented in a data frame with correct dimensions', {
  expect_is(factor_res, 'data.frame')
  expect_equal(length(factor_res), 4)
  expect_equal(length(factor_res[,1]), 27)
})

site_md_mod <- site_md
site_md_mod$si_igbp <- 'Not a valid level'
plant_md_mod <- plant_md
plant_md_mod$pl_social <- NA

factor_res_2 <- qc_factor_values(site_md_mod, stand_md, species_md,
                                 plant_md_mod, env_md)

test_that('results are correct', {
  expect_true(all(factor_res$Check_result))
  expect_false(all(factor_res$NA_presence))
  expect_false(factor_res_2$Check_result[3])
  expect_true(factor_res_2$NA_presence[9])
})

