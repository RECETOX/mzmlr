#' S3 class for mzML file representation
#'
#' The MzMlFile class provides a structured representation of an mzML file,
#' storing parsed XML data and metadata for efficient access to spectra and
#' chromatograms. Supports both local file paths and URLs.
#'
#' @param path Character string giving the path to the mzML file or a URL
#'   (http://, https://, ftp://)
#' @param xml_doc Parsed XML document (optional, for internal use)
#' @param validate Logical indicating whether to validate against XSD schema
#' @return An object of class "MzMlFile" with an additional `temp_file` component
#'   if the input was a URL (for cleanup purposes)
#'
#' @examples
#' \dontrun{
#' # Local file
#' mzml <- MzMlFile("path/to/file.mzML")
#'
#' # From URL
#' mzml <- MzMlFile("https://example.com/data/file.mzML")
#'
#' # Clean up temporary file after use
#' unlink(mzml$temp_file)
#' }
#'
#' @export
MzMlFile <- function(path, xml_doc = NULL, validate = TRUE) {
  # Check if path is a URL
  is_url <- grepl("^(https?|ftp)://", path, ignore.case = TRUE)

  temp_file <- NULL

  if (is_url) {
    # Download file to temporary location
    temp_file <- tempfile(fileext = ".mzML")
    on.exit(unlink(temp_file), add = TRUE)

    tryCatch(
      {
        utils::download.file(path, destfile = temp_file, mode = "wb", quiet = TRUE)
      },
      error = function(e) {
        cli::cli_abort("Failed to download file from URL: {.url {path}}\nError: {e$message}")
      }
    )

    # Use the downloaded file for processing
    actual_path <- temp_file
  } else {
    # Validate local file exists
    if (!file.exists(path)) {
      cli::cli_abort("File not found: {.file {path}}")
    }
    actual_path <- path
  }

  # Check file extension
  if (!grepl("\\.mzML$", actual_path, ignore.case = TRUE)) {
    cli::cli_warn("File does not have .mzML extension: {.file {actual_path}}")
  }

  # Parse XML document
  if (is.null(xml_doc)) {
    xml_doc <- .read_xml(actual_path, encoding = "UTF-8")
  }

  # Validate against schema if requested
  if (validate) {
    validation_result <- validate_mzml(actual_path)
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
  result <- structure(
    list(
      path = if (is_url) path else normalizePath(actual_path),
      original_path = path,
      temp_file = temp_file,
      xml = xml_doc,
      version = version,
      id = id,
      validated = validate,
      is_url = is_url
    ),
    class = "MzMlFile"
  )

  # Set up cleanup handler for URL-based files
  if (is_url) {
    reg.finalizer(environment(), function(e) {
      if (!is.null(e$temp_file) && file.exists(e$temp_file)) {
        unlink(e$temp_file)
      }
    }, onexit = FALSE)
  }

  result
}

#' Print method for MzMlFile objects
#'
#' @param x Object of class MzMlFile
#' @param ... Additional arguments (not used)
#'
#' @export
print.MzMlFile <- function(x, ...) {
  cat("<MzMlFile>\n")

  if (x$is_url) {
    cat("  URL: ", x$original_path, "\n", sep = "")
    cat("  Temp file: ", basename(x$temp_file), "\n", sep = "")
  } else {
    cat("  Path: ", x$path, "\n", sep = "")
  }

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
