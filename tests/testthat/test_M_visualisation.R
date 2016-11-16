library(sapfluxnetQC1)

################################################################################
context('M1. Biomes plot')

test_that('argument checks work', {
  expect_error(vis_biome(merge_deserts = 25),
               'merge_deserts must be logical')
  expect_error(vis_biome(merge_deserts = NA),
               'merge_deserts must be either TRUE or FALSE')
})

test_that('plot is created', {
  expect_is(vis_biome(), 'ggplot')
})

context('M2. Biomes plot with sites')

foo_data <- data.frame(
  si_code = c('AUS_MAR_UBW', 'BRA_CAM_', 'ESP_CAN_'),
  si_lat = c('-37.888', '-22.69', '41.4309888889'),
  si_long = c('145.572', '-45.52', '2.07361111111111'),
  si_mat = c(13.28, 13.96, 15.74),
  si_map = c(1166.43, 1782.68, 626.08)
)

test_that('argument checks work', {
  expect_error(vis_location_biome('data.frame'),
               'Provided data object is not a data.frame.')
  expect_error(vis_location_biome(subset(foo_data, select = -si_lat)),
               'There is no latitude variable in this dataset.')
  expect_error(vis_location_biome(subset(foo_data, select = -si_long)),
               'There is no longitude variable in this dataset.')
  expect_error(vis_location_biome(foo_data, merge_deserts = 25),
               'merge_deserts must be logical')
  expect_error(vis_location_biome(foo_data, merge_deserts = NA),
               'merge_deserts must be either TRUE or FALSE')
})

test_that('plot is created', {
  expect_is(vis_location_biome(foo_data), 'ggplot')
})
