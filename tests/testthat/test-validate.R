# Tests for validation functions

# Use a small test file with standard mzML 1.x format
TEST_FILE <- "tiny.pwiz.mzML0.99.10.mzML"

test_that("validate_mzml returns valid result for correct file", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  result <- validate_mzml(test_file)

  expect_true(result$valid)
  expect_equal(result$message, "Validation successful")
  expect_match(result$version, "^1\\.")
})

test_that("validate_mzml returns error for non-existent file", {
  result <- validate_mzml("non_existent_file.mzML")

  expect_false(result$valid)
  expect_match(result$message, "File not found")
})

test_that("validate_mzml checks required elements", {
  # Create a minimal invalid XML file
  tmp_file <- tempfile(fileext = ".mzML")
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<mzML version="1.1.0" xmlns="http://psi.hupo.org/ms/mzml">',
    '  <!-- Missing required elements -->',
    '</mzML>'
  ), tmp_file)

  on.exit(unlink(tmp_file))

  result <- validate_mzml(tmp_file)

  expect_false(result$valid)
  expect_match(result$message, "Missing required")
})

test_that("validate_mzml extracts version correctly", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  result <- validate_mzml(test_file)

  # Just check that version is extracted (may be 0.99.x format)
  expect_type(result$version, "character")
  expect_true(nchar(result$version) > 0)
})
