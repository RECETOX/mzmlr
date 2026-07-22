# Tests for validation functions

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
    "  <!-- Missing required elements -->",
    "</mzML>"
  ), tmp_file)

  on.exit(unlink(tmp_file))

  result <- validate_mzml(tmp_file)

  expect_false(result$valid)
  expect_match(result$message, "Missing required")
})

test_that("validate_mzml handles valid basic XML", {
  # Create a minimal valid XML file
  tmp_file <- tempfile(fileext = ".mzML")
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<mzML version="1.1.0" xmlns="http://psi.hupo.org/ms/mzml">',
    '  <cvList count="1"><cv id="MS" fullName="Test" version="1.0" URI="http://test"/></cvList>',
    '  <fileDescription><fileContent><cvParam cvRef="MS" accession="MS:1000579" name="MS1 spectrum"/></fileContent></fileDescription>',
    '  <softwareList count="1"><software id="test"/></softwareList>',
    '  <instrumentConfigurationList count="1"><instrumentConfiguration id="IC1"/></instrumentConfigurationList>',
    '  <dataProcessingList count="1"><dataProcessing id="DP1"/></dataProcessingList>',
    '  <run id="run1"></run>',
    "</mzML>"
  ), tmp_file)

  on.exit(unlink(tmp_file))

  result <- validate_mzml(tmp_file)

  expect_true(result$valid)
  expect_equal(result$message, "Validation successful")
  expect_equal(result$version, "1.1.0")
})

test_that("validate_mzml extracts version from valid file", {
  # Create a minimal valid XML file
  tmp_file <- tempfile(fileext = ".mzML")
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<mzML version="1.1.0" xmlns="http://psi.hupo.org/ms/mzml">',
    '  <cvList count="1"><cv id="MS" fullName="Test" version="1.0" URI="http://test"/></cvList>',
    '  <fileDescription><fileContent><cvParam cvRef="MS" accession="MS:1000579" name="MS1 spectrum"/></fileContent></fileDescription>',
    '  <softwareList count="1"><software id="test"/></softwareList>',
    '  <instrumentConfigurationList count="1"><instrumentConfiguration id="IC1"/></instrumentConfigurationList>',
    '  <dataProcessingList count="1"><dataProcessing id="DP1"/></dataProcessingList>',
    '  <run id="run1"></run>',
    "</mzML>"
  ), tmp_file)

  on.exit(unlink(tmp_file))

  result <- validate_mzml(tmp_file)

  expect_type(result$version, "character")
  expect_equal(result$version, "1.1.0")
})
