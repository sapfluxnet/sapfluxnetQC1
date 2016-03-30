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
