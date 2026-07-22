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
  skip_on_cran()
  # Skip if no internet connection
  if (!requireNamespace("curl", quietly = TRUE) ||
      curl::has_internet() == FALSE) {
    skip("No internet connection")
  }

  url <- 'https://zenodo.org/records/10618833/files/8_qc_no_dil_milliq.mzml.mzml?download=1'

  # Wrap in tryCatch to handle download failures
  result <- tryCatch({
    mzml <- read_mzml(url, validate = FALSE)
    list(success = TRUE, mzml = mzml)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })

  if (!result$success) {
    skip(paste("Download failed:", result$error))
  }

  mzml <- result$mzml
  expect_s3_class(mzml, "MzMlFile")
  expect_true(mzml$is_url)
  expect_type(mzml$temp_file, "character")
  expect_equal(mzml$original_path, url)
  # Index should be built for downloaded files
  expect_false(is.null(mzml$spectrum_index))
  expect_equal(mzml$spectrum_index$n_spectra, 4117)
})

test_that("validate_mzml handles URL format", {
  # Test that validation returns appropriate error for invalid URL
  result <- validate_mzml("https://invalid.url.that.does.not.exist/file.mzML")

  # Should fail to download
  expect_false(result$valid)
  expect_match(result$message, "Failed to download|File not found")
})
