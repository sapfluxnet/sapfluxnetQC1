library(sapfluxnetr)

context('B1. Coordinates argument checks')

test_that('argument errors are correct', {

  foo_data <- data.frame(
    si_long = rnorm(5),
    si_lat = rnorm(5),
    si_country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
    si_name = c('bla', 'ble', 'bli', 'blo', 'blu')
  )

  expect_error(qc_check_coordinates(foo_data, 'foo_folder'),
               'Maps folder does not exist')
  expect_error(qc_check_coordinates(c(1,2,3)),
               'Provided data object is not a data.frame')
  expect_error(qc_check_coordinates(foo_data[,2:4]), 'There is no longitude variable')
  expect_error(qc_check_coordinates(foo_data[,c(1, 3:4)]), 'There is no latitude variable')
  expect_error(qc_check_coordinates(foo_data[,c(1:2, 4)]), 'There is no country variable')
  expect_error(qc_check_coordinates(foo_data[,c(1:3)]), 'There is no site_name variable')
})

context('B2. Coordinates check')

test_that('checking is correct', {

  foo_data <- data.frame(
    si_long = c(-84, rnorm(4, 0, 0.1)),
    si_lat = c(10, rnorm(4, 0, 0.1)),
    si_country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
    si_name = c('bla', 'ble', 'bli', 'blo', 'blu')
  )
  qc_download_maps(foo_data)

  expect_message(qc_check_coordinates(foo_data[2:5,]), '4 wrong coordinates in data')
  expect_message(qc_check_coordinates(foo_data[1,]), '1 correct coordinates in data')
})

context('B3. Plotting wrong coordinates and report presence')

test_that('wrong coordinates are plotted correctly', {

  foo_data <- data.frame(
    si_long = c(-84, rnorm(4, 0, 0.1)),
    si_lat = c(10, rnorm(4, 0, 0.1)),
    si_country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
    si_name = c('bla', 'ble', 'bli', 'blo', 'blu')
  )
  foo_report <- qc_check_coordinates(foo_data[1:2,], plot = TRUE)

  expect_false(file_test("-f", 'CRI_bla.pdf'))
  expect_true(file_test("-f", 'ITA_ble.pdf'))
})

test_that('report is present and is a data.frame', {

  foo_data <- data.frame(
    si_long = c(-84, rnorm(4, 0, 0.1)),
    si_lat = c(10, rnorm(4, 0, 0.1)),
    si_country = c('CRI', 'ITA', 'NZL', 'CHE', 'BRA'),
    si_name = c('bla', 'ble', 'bli', 'blo', 'blu')
  )
  foo_report <- qc_check_coordinates(foo_data[1,])

  expect_is(foo_report, 'data.frame')
})

context('B4. Get biomes object')

test_that('argument checks work', {
  expect_error(qc_get_biomes_spdf(merge_deserts = 25),
               'merge_deserts must be logical')
  expect_error(qc_get_biomes_spdf(merge_deserts = NA),
               'merge_deserts must be either TRUE or FALSE')
})

test_that('biomes object is created', {
  expect_is(qc_get_biomes_spdf(), 'SpatialPolygonsDataFrame')
  expect_is(qc_get_biomes_spdf(merge_deserts = TRUE), 'SpatialPolygonsDataFrame')
})

context('B5. Get biome of a site')

foo_data <- data.frame(
  si_code = c('A', 'B', 'C', 'D'),
  si_lat = c(-36.785, 69.491822025, 41.4309888889, 34.3863888888889),
  si_long = c(146.582, 27.2310822944, 2.07361111111111, -106.529444444444)
)

results_foo_data <- cbind(foo_data, data.frame(
  si_mat = c(11.85414, -1.46320, 15.73861, 11.16112),
  si_map = c(1137.5661, 424.9901, 626.0845, 332.1532),
  si_biome = c('Temperate forest', 'Boreal forest', 'Mediterranean', 'Temperate grassland desert')
))

results_foo_data_md <- cbind(foo_data, data.frame(
  si_mat = c(11.85414, -1.46320, 15.73861, 11.16112),
  si_map = c(1137.5661, 424.9901, 626.0845, 332.1532),
  si_biome = c('Temperate forest', 'Boreal forest', 'Mediterranean', 'Desert')
))

test_that('argument checks work', {
  expect_error(qc_get_biome('foo'),
               'Provided data object is not a data frame.')
  expect_error(qc_get_biome(subset(foo_data, select=-si_long)),
               'There is no longitude variable in this dataset.')
  expect_error(qc_get_biome(subset(foo_data, select=-si_lat)),
               'There is no latitude variable in this dataset.')
  expect_error(qc_get_biome(foo_data,merge_deserts = 25),
               'merge_deserts must be logical')
  expect_error(qc_get_biome(foo_data,merge_deserts = NA),
               'merge_deserts must be either TRUE or FALSE')
})

test_that('the new variables are added and their type is correct', {
  expect_equal(
    c(names(foo_data), 'si_mat', 'si_map', 'si_biome'),
    names(qc_get_biome(foo_data))
  )
  expect_is(qc_get_biome(foo_data)$si_map, 'numeric')
  expect_is(qc_get_biome(foo_data)$si_mat, 'numeric')
  expect_is(qc_get_biome(foo_data)$si_biome, 'factor')
})

test_that('new data is correct', {
  expect_equal(qc_get_biome(foo_data), results_foo_data, tolerance = .0001)
  expect_equal(qc_get_biome(foo_data, merge_deserts = TRUE), results_foo_data_md, tolerance = .0001)
  expect_is(qc_get_biome(foo_data), 'data.frame')
})
