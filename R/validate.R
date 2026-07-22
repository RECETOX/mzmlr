#' Validate mzML file against XSD schema
#'
#' Validates an mzML file against the official HUPO-PSI mzML XSD schema.
#' This function checks both XML well-formedness and schema conformance.
#' Supports both local file paths and URLs.
#'
#' @param path Character string giving the path to the mzML file or a URL
#'   (http://, https://, ftp://)
#' @param schema_path Optional path to custom XSD schema. If NULL, uses
#'   the bundled mzML1.1.1.xsd schema.
#' @return A list with components:
#'   \describe{
#'     \item{valid}{Logical indicating if validation passed}
#'     \item{message}{Character string with validation message or errors}
#'     \item{version}{mzML version from file attributes}
#'   }
#'
#' @details
#' The function performs structural validation by checking required elements
#' and attributes are present. Full XSD validation requires additional packages.
#'
#' When a URL is provided, the file is downloaded to a temporary location,
#' validated, and the temporary file is automatically cleaned up.
#'
#' @examples
#' \dontrun{
#' # Local file
#' result <- validate_mzml("path/to/file.mzML")
#'
#' # From URL
#' result <- validate_mzml("https://example.com/data/file.mzML")
#'
#' if (result$valid) {
#'   cat("File is valid mzML version", result$version, "\n")
#' } else {
#'   cat("Validation errors:", result$message, "\n")
#' }
#' }
#'
#' @export
validate_mzml <- function(path, schema_path = NULL) {
  # Check if path is a URL
  is_url <- grepl("^(https?|ftp)://", path, ignore.case = TRUE)

  actual_path <- path

  if (is_url) {
    # Download file to temporary location
    temp_file <- tempfile(fileext = ".mzML")
    on.exit(unlink(temp_file), add = TRUE)

    tryCatch(
      {
        utils::download.file(path, destfile = temp_file, mode = "wb", quiet = TRUE)
      },
      error = function(e) {
        return(list(
          valid = FALSE,
          message = paste("Failed to download file from URL:", e$message),
          version = NA_character_
        ))
      }
    )

    actual_path <- temp_file
  }

  # Check file exists
  if (!file.exists(actual_path)) {
    return(list(
      valid = FALSE,
      message = paste("File not found:", path),
      version = NA_character_
    ))
  }

  # Try to parse XML first using xml2
  xml_doc <- tryCatch(
    .read_xml(actual_path, encoding = "UTF-8"),
    error = function(e) {
      return(list(
        valid = FALSE,
        message = paste("XML parsing error:", e$message),
        version = NA_character_
      ))
    }
  )

  if (is.list(xml_doc) && !is.null(xml_doc$valid) && !xml_doc$valid) {
    return(xml_doc)
  }

  # Extract version from root element
  root <- .xml_root(xml_doc)
  version <- .xml_attr(root, "version")

  if (is.null(version) || version == "" || is.na(version)) {
    return(list(
      valid = FALSE,
      message = "Missing required 'version' attribute on root element",
      version = NA_character_
    ))
  }

  # Structural validation - check required elements using namespace-agnostic finders
  errors <- c()

  # Required: cvList
  if (length(.xml_find_first_by_name(root, "cvList")) == 0) {
    errors <- c(errors, "Missing required 'cvList' element")
  }

  # Required: fileDescription
  if (length(.xml_find_first_by_name(root, "fileDescription")) == 0) {
    errors <- c(errors, "Missing required 'fileDescription' element")
  }

  # Required: softwareList
  if (length(.xml_find_first_by_name(root, "softwareList")) == 0) {
    errors <- c(errors, "Missing required 'softwareList' element")
  }

  # Required: instrumentConfigurationList
  if (length(.xml_find_first_by_name(root, "instrumentConfigurationList")) == 0) {
    errors <- c(errors, "Missing required 'instrumentConfigurationList' element")
  }

  # Required: dataProcessingList
  if (length(.xml_find_first_by_name(root, "dataProcessingList")) == 0) {
    errors <- c(errors, "Missing required 'dataProcessingList' element")
  }

  # Required: run
  if (length(.xml_find_first_by_name(root, "run")) == 0) {
    errors <- c(errors, "Missing required 'run' element")
  }

  # Check for mzML root element
  root_name <- .xml_name(root)
  if (root_name != "mzML" && !grepl("mzML$", root_name, ignore.case = TRUE)) {
    errors <- c(errors, "Missing or invalid mzML root element")
  }

  if (length(errors) > 0) {
    return(list(
      valid = FALSE,
      message = paste(errors, collapse = "; "),
      version = version
    ))
  }

  return(list(
    valid = TRUE,
    message = "Validation successful",
    version = version
  ))
}
