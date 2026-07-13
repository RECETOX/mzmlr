# Tests for info functions

# Use a small test file
TEST_FILE <- "tiny1.mzML0.99.1.mzML"

test_that("get_file_info returns expected structure", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  info <- get_file_info(mzml)

  expect_type(info, "list")
  expect_named(info, c(
    "path", "version", "id", "file_content",
    "source_files", "software", "spectrum_count",
    "chromatogram_count", "start_time", "ms_levels"
  ))

  # Check path is normalized
  expect_equal(info$path, normalizePath(test_file))

  # Check version matches file
  expect_equal(info$version, mzml$version)
})

test_that("get_file_info errors for non-MzMlFile object", {
  expect_error(get_file_info("not an MzMlFile"), "MzMlFile object")
  expect_error(get_file_info(list()), "MzMlFile object")
})

test_that("get_instrument_info returns expected structure", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  instr <- get_instrument_info(mzml)

  expect_type(instr, "list")
  expect_named(instr, c("instrument_id", "components", "serial_number", "manufacturer"))

  # Components should be a list
  expect_type(instr$components, "list")
})

test_that("get_ms_levels returns integer vector", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  levels <- get_ms_levels(mzml)

  expect_type(levels, "integer")
  # MS levels should be positive integers if present
  if (length(levels) > 0) {
    expect_true(all(levels > 0))
  }
})

test_that("get_file_info extracts source file information", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  info <- get_file_info(mzml)

  # Source files should be a list of lists
  expect_type(info$source_files, "list")

  if (length(info$source_files) > 0) {
    first_source <- info$source_files[[1]]
    expect_true(any(c("id", "name", "location") %in% names(first_source)))
  }
})

test_that("get_file_info extracts software information", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  info <- get_file_info(mzml)

  expect_type(info$software, "list")

  if (length(info$software) > 0) {
    first_software <- info$software[[1]]
    expect_true(any(c("id", "version") %in% names(first_software)))
  }
})
