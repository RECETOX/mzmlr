# Tests for reading functions

# Use a small test file
TEST_FILE <- "tiny1.mzML0.99.1.mzML"

test_that("read_mzml creates MzMlFile object", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)

  expect_s3_class(mzml, "MzMlFile")
  expect_equal(mzml$path, normalizePath(test_file))
  expect_true(!is.null(mzml$xml))
  expect_true(!is.null(mzml$version))
})

test_that("read_mzml validates by default", {
  # Create invalid XML file
  tmp_file <- tempfile(fileext = ".mzML")
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<mzML version="1.1.0" xmlns="http://psi.hupo.org/ms/mzml">',
    '  <!-- Missing required elements -->',
    '</mzML>'
  ), tmp_file)

  on.exit(unlink(tmp_file))

  # Should fail validation by default
  expect_error(read_mzml(tmp_file, validate = TRUE), "Validation failed")

  # Should work with validation disabled
  mzml <- expect_no_error(read_mzml(tmp_file, validate = FALSE))
  expect_s3_class(mzml, "MzMlFile")
})

test_that("read_mzml errors for non-existent file", {
  expect_error(
    read_mzml("non_existent_file.mzML"),
    "File not found"
  )
})

test_that("print.MzMlFile displays information", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)

  # Capture print output
  output <- capture.output(print(mzml))

  expect_true(any(grepl("MzMlFile", output)))
  expect_true(any(grepl("Path|Version", output, ignore.case = TRUE)))
})
