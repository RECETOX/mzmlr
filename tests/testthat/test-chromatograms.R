# Tests for chromatogram functions

# Use a small test file
TEST_FILE <- "tiny2_SRM.mzML0.99.1.mzML"

test_that("get_chromatograms returns list (may be empty)", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  chrons <- get_chromatograms(mzml)

  # Should return a list (possibly empty)
  expect_type(chrons, "list")
})

test_that("get_chromatogram errors for non-MzMlFile object", {
  expect_error(get_chromatogram("not an MzMlFile", 1), "MzMlFile object")
})

test_that("get_chromatograms errors for non-MzMlFile object", {
  expect_error(get_chromatograms("not an MzMlFile"), "MzMlFile object")
})

test_that("get_chromatogram errors for invalid index", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  chrons <- get_chromatograms(mzml)

  if (length(chrons) > 0) {
    # Negative index
    expect_error(get_chromatogram(mzml, -1), "out of range")

    # Zero index
    expect_error(get_chromatogram(mzml, 0), "out of range")

    # Too large index
    expect_error(get_chromatogram(mzml, 1e9), "out of range")
  } else {
    skip("File has no chromatograms to test indexing")
  }
})

test_that("get_chromatogram returns expected structure when chromatograms exist", {
  test_file <- system.file("extdata", TEST_FILE, package = "mzmlr")

  if (test_file == "") {
    skip("Test mzML file not found in inst/extdata")
  }

  mzml <- read_mzml(test_file, validate = FALSE)
  chrons <- get_chromatograms(mzml)

  if (length(chrons) > 0) {
    chrom <- get_chromatogram(mzml, 1)

    expect_type(chrom, "list")
    expect_named(chrom, c(
      "time", "intensity", "id",
      "transition_list", "chromatogram_type"
    ))
  } else {
    skip("File has no chromatograms to test structure")
  }
})
