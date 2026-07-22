#' S3 class for mzML file representation
#'
#' The MzMlFile class provides a structured representation of an mzML file,
#' storing parsed XML data and metadata for efficient access to spectra and
#' chromatograms. Supports both local file paths and URLs.
#'
#' When reading large files, the package can build an index of spectrum positions
#' to enable memory-efficient random access without loading the entire XML into memory.
#'
#' @param path Character string giving the path to the mzML file or a URL
#'   (http://, https://, ftp://)
#' @param xml_doc Parsed XML document (optional, for internal use)
#' @param validate Logical indicating whether to validate against XSD schema
#' @param build_index Logical; if TRUE, build a spectrum position index for
#'   memory-efficient access (default TRUE for files > 10MB)
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
#' # Build index for large file
#' mzml <- MzMlFile("large_file.mzML", build_index = TRUE)
#'
#' # Clean up temporary file after use
#' unlink(mzml$temp_file)
#' }
#'
#' @export
MzMlFile <- function(path, xml_doc = NULL, validate = TRUE, build_index = NULL) {
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

  # Get file size for auto-indexing decision
  file_size <- file.info(actual_path)$size
  auto_build_index <- is.null(build_index) && file_size > 10 * 1024 * 1024 # 10MB threshold

  # Decide whether to build index
  should_build_index <- if (is.null(build_index)) auto_build_index else build_index

  # Parse XML document using xml2 (only if not building index-only mode)
  xml_doc <- NULL
  version <- NA_character_
  id <- NA_character_

  if (!should_build_index || !is_url) {
    # For local files or when index shouldn't replace XML, parse XML
    xml_doc <- .read_xml(actual_path, encoding = "UTF-8")

    # Extract basic metadata using xml2
    root <- .xml_root(xml_doc)
    version <- .xml_attr(root, "version")
    id <- .xml_attr(root, "id")

    # Clean up NA values
    if (is.null(version) || is.na(version) || version == "") version <- NA_character_
    if (is.null(id) || is.na(id) || id == "") id <- NA_character_
  }

  # Validate against schema if requested
  if (validate) {
    validation_result <- validate_mzml(actual_path)
    if (!validation_result$valid) {
      cli::cli_abort("Validation failed: {validation_result$message}")
    }
  }

  # Build spectrum index if requested
  # For URL downloads, the temp file exists during the session so we can build the index
  spectrum_index <- NULL
  if (should_build_index) {
    spectrum_index <- .build_spectrum_index(actual_path)
  }

  # Create and return object
  result <- structure(
    list(
      path = if (is_url) path else normalizePath(actual_path),
      original_path = path,
      temp_file = temp_file,
      xml = xml_doc,
      spectrum_index = spectrum_index,
      version = version,
      id = id,
      validated = validate,
      is_url = is_url,
      file_size = file_size
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

  # Report spectrum count
  if (!is.null(x$spectrum_index)) {
    cat("  Spectra: ", x$spectrum_index$n_spectra, " (indexed)", "\n", sep = "")
  } else if (!is.null(x$xml)) {
    root <- .xml_root(x$xml)
    sl_node <- .xml_find_first_by_name(root, "spectrumList")
    if (length(sl_node) > 0) {
      count_attr <- .xml_attr(sl_node, "count")
      if (!is.na(count_attr) && count_attr != "") {
        cat("  Spectra: ", count_attr, "\n", sep = "")
      }
    }
  }

  cat("  File size: ", format(x$file_size, big.mark = ","), " bytes\n", sep = "")

  cat("\n")
}
