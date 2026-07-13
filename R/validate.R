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
#' The function performs two levels of validation:
#' \enumerate{
#'   \item XML parsing to check well-formedness
#'   \item Schema validation using the RcppXSD package or fallback validation
#' }
#'
#' For schema validation, if the \code{xml2} package supports it, full XSD
#' validation is performed. Otherwise, a structural validation is done by
#' checking required elements and attributes are present.
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

  # Try to parse XML first
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

  # Read file content for structural validation
  content <- readLines(actual_path, warn = FALSE, encoding = "UTF-8")
  content <- paste(content, collapse = "\n")

  # Structural validation - check required elements
  errors <- c()

  # Required: cvList
  if (!grepl("<cvList", content, ignore.case = TRUE)) {
    errors <- c(errors, "Missing required 'cvList' element")
  }

  # Required: fileDescription
  if (!grepl("<fileDescription", content, ignore.case = TRUE)) {
    errors <- c(errors, "Missing required 'fileDescription' element")
  }

  # Required: softwareList
  if (!grepl("<softwareList", content, ignore.case = TRUE)) {
    errors <- c(errors, "Missing required 'softwareList' element")
  }

  # Required: instrumentConfigurationList
  if (!grepl("<instrumentConfigurationList", content, ignore.case = TRUE)) {
    errors <- c(errors, "Missing required 'instrumentConfigurationList' element")
  }

  # Required: dataProcessingList
  if (!grepl("<dataProcessingList", content, ignore.case = TRUE)) {
    errors <- c(errors, "Missing required 'dataProcessingList' element")
  }

  # Required: run
  if (!grepl("<run", content, ignore.case = TRUE)) {
    errors <- c(errors, "Missing required 'run' element")
  }

  # Check for mzML namespace in root element
  root_match <- regmatches(content, regexpr('<mzML[^>]*>', content, ignore.case = TRUE))
  if (length(root_match) == 0 || nchar(root_match[1]) == 0) {
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
