#' Read mzML file into R
#'
#' Reads an mzML file from a local path or URL and returns a structured
#' MzMlFile object containing parsed XML data and metadata.
#'
#' @param path Character string giving the path to the mzML file or a URL
#'   (http://, https://, ftp://)
#' @param validate Logical; whether to validate against XSD schema (default TRUE)
#' @param lazy Logical; if TRUE, only parse metadata and defer spectrum/chromatogram
#'   parsing until requested (default TRUE for memory efficiency)
#' @param build_index Logical; if TRUE, build a spectrum position index for
#'   memory-efficient random access. If NULL (default), index is built automatically
#'   for files larger than 10MB.
#'
#' @return An object of class [MzMlFile()] with the following components:
#'   \describe{
#'     \item{path}{Absolute path to the mzML file or the original URL}
#'     \item{original_path}{The original path or URL provided}
#'     \item{temp_file}{Temporary file path if input was a URL (NULL for local files)}
#'     \item{xml}{Parsed XML document (NULL if index-only mode)}
#'     \item{spectrum_index}{Spectrum position index (if built)}
#'     \item{version}{mzML schema version}
#'     \item{id}{File identifier from root element}
#'     \item{validated}{Whether validation was performed}
#'     \item{is_url}{Logical indicating if the input was a URL}
#'     \item{file_size}{File size in bytes}
#'   }
#'
#' @details
#' When reading from a URL, the file is downloaded to a temporary location,
#' parsed, and the temporary file is tracked for cleanup. The temporary file
#' will be automatically removed when the R session ends, but you can manually
#' remove it with \code{unlink(mzml$temp_file)}.
#'
#' The function uses lazy loading by default, meaning that binary spectrum
#' data is not decoded until explicitly requested via [get_spectrum()],
#' [get_spectra()], or [get_chromatograms()]. This significantly reduces
#' memory usage for large files.
#'
#' For files larger than 10MB, an index of spectrum positions is built automatically.
#' This index enables memory-efficient random access to spectra without loading
#' the entire XML document into memory. Set `build_index = FALSE` to disable this
#' behavior, or `build_index = TRUE` to force indexing for smaller files.
#'
#' Binary data in mzML files is typically zlib-compressed and encoded in
#' base64. The decoding is handled automatically when accessing spectrum data.
#'
#' @examples
#' \dontrun{
#' # Basic reading from local file
#' mzml <- read_mzml("path/to/file.mzML")
#'
#' # Reading from URL
#' mzml <- read_mzml("https://example.com/data/file.mzML")
#'
#' # Force index building for small file
#' mzml <- read_mzml("small_file.mzML", build_index = TRUE)
#'
#' # Disable index building
#' mzml <- read_mzml("large_file.mzML", build_index = FALSE)
#'
#' # Get file information
#' info <- get_file_info(mzml)
#'
#' # Get all spectra (may use significant memory)
#' spectra <- get_spectra(mzml)
#'
#' # Get single spectrum by index
#' spec <- get_spectrum(mzml, 1)
#'
#' # Clean up temporary file after use (for URL-based reads)
#' if (!is.null(mzml$temp_file)) {
#'   unlink(mzml$temp_file)
#' }
#' }
#'
#' @export
read_mzml <- function(path, validate = TRUE, lazy = TRUE, build_index = NULL) {
  MzMlFile(path = path, xml_doc = NULL, validate = validate, build_index = build_index)
}
