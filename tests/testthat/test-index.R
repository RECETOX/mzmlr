# Tests for spectrum indexing functionality

TEST_FILE <- "tiny.pwiz.mzML0.99.10.mzML"

test_that("spectrum index is built correctly", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  # Build index directly (using ::: to access internal function)
  index <- mzmlr:::.build_spectrum_index(test_file)

  expect_type(index, "list")
  expect_named(index, c("positions", "end_positions", "n_spectra", "file_size"))
  expect_equal(index$n_spectra, 2L)
})

test_that("group consecutive indices works", {
  result <- mzmlr:::.group_consecutive_indices(1:5)
  expect_length(result, 1)

  # Non-consecutive with default max_gap=100 will be grouped together
  result <- mzmlr:::.group_consecutive_indices(c(1, 10, 20))
  expect_length(result, 1)

  # With smaller max_gap
  result <- mzmlr:::.group_consecutive_indices(c(1, 10, 20), max_gap = 5)
  expect_length(result, 3)
})
