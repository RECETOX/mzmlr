#' S3 class for mzML file representation
#'
#' The MzMlFile class provides a structured representation of an mzML file,
#' storing parsed XML data and metadata for efficient access to spectra and
#' chromatograms.
#'
#' @param path Character string giving the path to the mzML file
#' @param xml_doc Parsed XML document (optional, for internal use)
#' @param validate Logical indicating whether to validate against XSD schema
#' @return An object of class "MzMlFile"
#'
#' @examples
#' \dontrun{
#' mzml <- MzMlFile("path/to/file.mzML")
#' info <- get_file_info(mzml)
#' }
#'
#' @export
MzMlFile <- function(path, xml_doc = NULL, validate = TRUE) {
  # Validate file exists
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }

  # Check file extension
  if (!grepl("\\.mzML$", path, ignore.case = TRUE)) {
    cli::cli_warn("File does not have .mzML extension: {.file {path}}")
  }

  # Parse XML document
  if (is.null(xml_doc)) {
    xml_doc <- .read_xml(path, encoding = "UTF-8")
  }

  # Validate against schema if requested
  if (validate) {
    validation_result <- validate_mzml(path)
    if (!validation_result$valid) {
      cli::cli_abort("Validation failed: {validation_result$message}")
    }
  }

  # Extract basic metadata - handle both xml2 and base R parsing
  version <- NA_character_
  id <- NA_character_

  if (inherits(xml_doc, "xml_document")) {
    # xml2 package
    root <- xml2::xml_root(xml_doc)
    version <- xml2::xml_attr(root, "version")
    id <- xml2::xml_attr(root, "id")
  } else if (inherits(xml_doc, "mzml_xml_base") && !is.null(xml_doc$root)) {
    # Base R fallback
    version <- xml_doc$root[["version"]]
    id <- xml_doc$root[["id"]]
  }

  # Clean up NA values
  if (is.null(version) || is.na(version) || version == "") version <- NA_character_
  if (is.null(id) || is.na(id) || id == "") id <- NA_character_

  # Create and return object
  structure(
    list(
      path = normalizePath(path),
      xml = xml_doc,
      version = version,
      id = id,
      validated = validate
    ),
    class = "MzMlFile"
  )
}

#' Print method for MzMlFile objects
#'
#' @param x Object of class MzMlFile
#' @param ... Additional arguments (not used)
#'
#' @export
print.MzMlFile <- function(x, ...) {
  cat("<MzMlFile>\n")
  cat("  Path: ", x$path, "\n", sep = "")
  cat("  Version: ", ifelse(is.na(x$version), "unknown", x$version), "\n", sep = "")
  if (!is.na(x$id)) {
    cat("  ID: ", x$id, "\n", sep = "")
  }

  # Get counts without full parsing
  content <- x$xml$content
  if (is.character(content) && length(content) == 1 && nchar(content) > 0) {
    sl_pattern <- '<spectrumList[^>]*count=\"([^\"]+)\"'
    sl_pos <- regexec(sl_pattern, content, ignore.case = TRUE, perl = TRUE)[[1]]
    if (sl_pos[1] > 0) {
      sl_match <- regmatches(content, list(sl_pos))[[1]]
      if (length(sl_match) >= 2 && nchar(sl_match[2]) > 0) {
        cat("  Spectra: ", sl_match[2], "\n", sep = "")
      }
    }
  }

  cat("\n")
}
