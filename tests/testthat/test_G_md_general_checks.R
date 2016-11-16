library(sapfluxnetQC1)

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

plant_md$pl_sens_calib <- TRUE
plant_check <- qc_md_cols(plant_md, 'plant_md')
plant_md_mod <- plant_md
plant_md_mod$pl_name <- 1:13
plant_md_mod_2 <- plant_md
plant_md_mod_2$pl_species <- 1:13
plant_mod_check <- qc_md_cols(plant_md_mod, 'plant_md')
plant_mod_2_check <- qc_md_cols(plant_md_mod_2, 'plant_md')
### añadir test para el resto de metadata

test_that('results are correct', {
  expect_true(all(site_check$PresenceOK))
  expect_false(all(site_check$ClassOK))
  expect_true(all(site_mod_check$ClassOK))
  expect_false(site_mod_2_check$ClassOK[1])

  expect_true(all(plant_check$PresenceOK))
  expect_true(all(plant_check$ClassOK[-3]))
  expect_true(all(plant_mod_check$ClassOK[-3]))
  expect_false(all(plant_mod_2_check$ClassOK[-3]))
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

context('G3. Environmental data presence')

env_data <- dl_data(file_name, 'environmental_hd')
env_md <- dl_metadata(file_name, 'environmental_md', si_code_loc = site_md)
good_env_presence <- qc_env_vars_presence(env_data, env_md)
bad_env_presence <- qc_env_vars_presence(env_data, site_md)
env_data$ta <- NULL
env_md$rh <- NA
regular_env_presence <- qc_env_vars_presence(env_data, env_md)
env_data$ta <- NA
na_env_presence <- qc_env_vars_presence(env_data, env_md)


test_that('argument checks work', {
  expect_error(qc_env_vars_presence('not a dataframe', env_md),
                 'Data and/or metadata objects are not data frames')
  expect_error(qc_env_vars_presence(env_data, 'not a data frame'),
                 'Data and/or metadata objects are not data frames')
  expect_error(qc_env_vars_presence('not a data frame', 'not a data frame'),
                 'Data and/or metadata objects are not data frames')
})

test_that('result is a data frame', {
  expect_is(good_env_presence, 'data.frame')
})

test_that('results are correct', {
  expect_true(all(good_env_presence$Md_presence == good_env_presence$Data_presence))
  expect_true(all(good_env_presence$Concordance))
  expect_false(all(bad_env_presence$Md_presence == bad_env_presence$Data_presence))
  expect_false(all(bad_env_presence$Concordance))
  expect_false(all(bad_env_presence$Md_presence))
  expect_false(all(regular_env_presence$Md_presence == regular_env_presence$Data_presence))
  expect_false(all(regular_env_presence$Concordance))
  expect_true(any(regular_env_presence$Concordance))
  expect_false(na_env_presence$Concordance[na_env_presence$Name == 'env_ta'])
})


context('G4. Species names checks')

species <- c('Olea europaea', 'Pinus pinaster', 'Pinus halepensis',
             'Eucalyptus globulus', 'Fagus sylvatica')

species_bad <- c('Oleae eurpea', 'Pino pinaster ', ' Pinu alepensis',
                 'Euclaiptus gobulus', 'Fagus silvatica')

species_not_so_bad <- c('Olea europaee', 'Pinus pinaster', 'Pinus halepensis',
                        'Eucaliptus globulus ', ' Fagos sylvatica')

good_info <- qc_species_names_info(species)
bad_info <- qc_species_names_info(species_bad)
not_so_bad_info <- qc_species_names_info(species_not_so_bad)

test_that('results of info are data frames', {
  expect_true(is.data.frame(good_info))
  expect_true(is.data.frame(bad_info))
  expect_true(is.data.frame(not_so_bad_info))
})

test_that('trimming works', {
  expect_equal(good_info$data_names, species)
  expect_equal(good_info$tpl_names, species)
  expect_true(all(good_info$Concordance))
  expect_equal(bad_info$data_names, c('Oleae eurpea', 'Pino pinaster',
                                      'Pinu alepensis', 'Euclaiptus gobulus',
                                      'Fagus silvatica'))
  expect_equal(bad_info$tpl_names, c(NA, NA, 'Pinus alepensis', NA,
                                     'Fagus sylvatica'))
  expect_false(all(bad_info$Concordance))
  expect_equal(not_so_bad_info$data_names, c('Olea europaee', 'Pinus pinaster',
                                             'Pinus halepensis', 'Eucaliptus globulus',
                                             'Fagos sylvatica'))
  expect_equal(not_so_bad_info$tpl_names, species)
  expect_false(all(not_so_bad_info$Concordance))
})

good_res <- qc_species_names(species)
bad_res <- suppressWarnings(qc_species_names(species_bad))
not_so_bad_res <- qc_species_names(species_not_so_bad)

test_that('results are the same if no change is needed', {
  expect_equal(as.character(good_res), species)
  expect_equal(as.character(bad_res), stringr::str_trim(species_bad, 'both'))
  expect_equal(as.character(not_so_bad_res), species)
})

context('G5. Email checks')

emails <- data.frame(
  si_contact_email = c(
    'victorgrandagarcia@gmail.com', 'v.granda@creaf.uab.cat',
    '123@123', 'Victor.Granda.Garcia@yahoo.es',
    'turururu@ailsdhflashf.ajshdjsdh', 'df.aD@c.123',
    '-df.aD@c.123', 'yooo.yooo'
  ),
  si_addcontr_email = c(
    NA,NA,NA,NA,NA,NA,NA,NA
  ),
  stringsAsFactors = FALSE
)

emilios_res <- qc_email_check(emails)

test_that('result is a data frame', {
  expect_is(emilios_res, 'data.frame')
})

test_that('results are correct', {
  expect_equal(emilios_res$Is_correct,
               c(TRUE, TRUE, FALSE, TRUE,
                 FALSE, FALSE, FALSE, FALSE,
                 NA, NA, NA, NA,
                 NA, NA, NA, NA))
})
