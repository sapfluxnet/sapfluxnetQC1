library(sapfluxnetr)

context('Argument checks')

test_that('arguments error are correct', {

  foo_data <- data.frame(x = rnorm(5,1,1),
                         country = c(rep('IRE', 2), rep('IRL', 3)))

  foo_wrong_data <- data.frame(x = rnorm(5,1,1),
                               y = rpois(5,1))

  expect_error(download_maps(foo_wrong_data, getwd()), 'There is no country')
  expect_error(download_maps(c(1,2,3), getwd()), 'object is not a data.frame')
  expect_error(download_maps(foo_data, 'foo_folder'),
                 'Destination folder does not exist.')
})
