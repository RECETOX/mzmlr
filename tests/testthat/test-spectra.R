# Tests for spectrum functions

# Use a small test file with spectra
TEST_FILE <- "tiny.pwiz.mzML0.99.10.mzML"

test_that("get_spectrum returns expected structure", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  spec <- get_spectrum(mzml, 1)

  expect_type(spec, "list")
  expect_named(spec, c(
    "mz", "intensity", "id", "ms_level",
    "scan_time", "total_ion_current",
    "base_peak_mz", "base_peak_intensity"
  ))

  # mz and intensity should be numeric vectors
  expect_type(spec$mz, "double")
  expect_type(spec$intensity, "double")

  # Should have same length
  expect_equal(length(spec$mz), length(spec$intensity))
})

test_that("get_spectrum errors for invalid index", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)

  # Negative index
  expect_error(get_spectrum(mzml, -1), "out of range")

  # Zero index
  expect_error(get_spectrum(mzml, 0), "out of range")

  # Too large index
  expect_error(get_spectrum(mzml, 1e9), "out of range")
})

test_that("get_spectrum errors for non-MzMlFile object", {
  expect_error(get_spectrum("not an MzMlFile", 1), "MzMlFile object")
})

test_that("get_spectra returns list of spectra", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  specs <- get_spectra(mzml, indices = 1:3)

  expect_type(specs, "list")
  expect_length(specs, 3)

  # Each spectrum should have the expected structure
  for (spec in specs) {
    expect_true(all(c("mz", "intensity", "id") %in% names(spec)))
  }
})

test_that("get_spectra with ms_level filter works", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)

  # Get MS1 spectra only
  ms1_specs <- get_spectra(mzml, ms_level = 1)

  expect_type(ms1_specs, "list")

  # All returned spectra should be MS1
  for (spec in ms1_specs) {
    expect_equal(spec$ms_level, 1L)
  }
})

test_that("spectrum data contains valid values", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  spec <- get_spectrum(mzml, 1)

  # m/z values should be positive (if we successfully decoded them)
  if (length(spec$mz) > 0) {
    expect_true(all(spec$mz > 0, na.rm = TRUE))

    # Intensities should be non-negative
    expect_true(all(spec$intensity >= 0, na.rm = TRUE))

    # m/z should be increasing (typical for mass spectra)
    if (length(spec$mz) > 1) {
      expect_true(all(diff(spec$mz) >= 0))
    }
  }
})
