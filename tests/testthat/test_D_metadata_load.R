library(sapfluxnetr)

context('D1. Metadata load')

foo_object <- 25

# foo file
zz <- file("foo.data", "w")  # open an output file connection
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
cat("One more line\n", file = zz)
close(zz)

test_that('Error raises in case of bad arguments', {
  expect_error(dl_metadata('file_that_not_exists', 'site_md'),
               'File does not exist')
  expect_error(dl_metadata(foo_object, 'site_md'),
               'File name is not provided as character')
  expect_error(dl_metadata('foo.data', 'not_a_metadata_sheet'),
               'Provided sheet name is not a character or is not a metadata')
  expect_error(dl_metadata('foo.data', foo_object),
               'Provided sheet name is not a character or is not a metadata')
})

file.remove('foo.data')

# xlsx file
xlsx_name <- 'foo.xlsx'

site_md <- suppressMessages(dl_metadata(xlsx_name, 'site_md'))
stand_md <- dl_metadata(xlsx_name, 'stand_md', si_code_loc = site_md)
species_md <- dl_metadata(xlsx_name, 'species_md', si_code_loc = site_md)
plant_md <- dl_metadata(xlsx_name, 'plant_md', si_code_loc = site_md)
env_md <- dl_metadata(xlsx_name, 'environmental_md', si_code_loc = site_md)

test_that('function returns data frames', {
  expect_is(site_md, 'data.frame')
  expect_is(stand_md, 'data.frame')
  expect_is(species_md, 'data.frame')
  expect_is(plant_md, 'data.frame')
  expect_is(env_md, 'data.frame')
})

test_that('si_code is correctly inserted in all metadata', {
  expect_identical(site_md$si_code, 'ESP_VAL_SOR')
  expect_identical(stand_md$si_code, 'ESP_VAL_SOR')
  expect_true(all(species_md$si_code == 'ESP_VAL_SOR'))
  expect_true(all(plant_md$si_code == 'ESP_VAL_SOR'))
  expect_identical(env_md$si_code, 'ESP_VAL_SOR')
})

test_that('each metadata object has the correct number of variables', {
  expect_equal(length(names(site_md)), 20)
  expect_equal(length(names(stand_md)), 17)
  expect_equal(length(names(species_md)), 5)
  expect_equal(length(names(plant_md)), 24)
  expect_equal(length(names(env_md)), 17)
})
