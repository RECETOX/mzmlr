# Tests for URL support

test_that("URL detection works correctly", {
  # Test URL patterns
  urls <- c(
    "https://example.com/file.mzML",
    "http://example.com/file.mzML",
    "ftp://example.com/file.mzML"
  )

  for (url in urls) {
    expect_true(grepl("^(https?|ftp)://", url, ignore.case = TRUE))
  }

  # Test non-URL patterns
  paths <- c(
    "/local/path/file.mzML",
    "relative/path/file.mzML",
    "file.mzML"
  )

  for (path in paths) {
    expect_false(grepl("^(https?|ftp)://", path, ignore.case = TRUE))
  }
})

test_that("read_mzml from URL creates MzMlFile with temp_file", {
  skip_on_ci()
  url <- 'https://zenodo.org/records/10618833/files/8_qc_no_dil_milliq.mzml.mzml'
  mzml <- read_mzml(url, validate = FALSE)

  expect_s3_class(mzml, "MzMlFile")
  expect_true(mzml$is_url)
  expect_type(mzml$temp_file, "character")
  expect_equal(mzml$original_path, url)
})

test_that("validate_mzml handles URL format", {
  # Test that validation returns appropriate error for invalid URL
  result <- validate_mzml("https://invalid.url.that.does.not.exist/file.mzML")

  # Should fail to download
  expect_false(result$valid)
  expect_match(result$message, "Failed to download|File not found")
})
