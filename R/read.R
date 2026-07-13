#' Read mzML file into R
#'
#' Reads an mzML file and returns a structured MzMlFile object containing
#' parsed XML data and metadata.
#'
#' @param path Character string giving the path to the mzML file
#' @param validate Logical; whether to validate against XSD schema (default TRUE)
#' @param lazy Logical; if TRUE, only parse metadata and defer spectrum/chromatogram
#'   parsing until requested (default TRUE for memory efficiency)
#'
#' @return An object of class [MzMlFile()] with the following components:
#'   \describe{
#'     \item{path}{Absolute path to the mzML file}
#'     \item{xml}{Parsed XML document}
#'     \item{version}{mzML schema version}
#'     \item{id}{File identifier from root element}
#'     \item{validated}{Whether validation was performed}
#'   }
#'
#' @details
#' The function uses lazy loading by default, meaning that binary spectrum
#' data is not decoded until explicitly requested via [get_spectrum()],
#' [get_spectra()], or [get_chromatograms()]. This significantly reduces
#' memory usage for large files.
#'
#' Binary data in mzML files is typically zlib-compressed and encoded in
#' base64. The decoding is handled automatically when accessing spectrum data.
#'
#' @examples
#' \dontrun{
#' # Basic reading
#' mzml <- read_mzml("path/to/file.mzML")
#'
#' # Get file information
#' info <- get_file_info(mzml)
#'
#' # Get all spectra (may use significant memory)
#' spectra <- get_spectra(mzml)
#'
#' # Get single spectrum by index
#' spec <- get_spectrum(mzml, 1)
#' }
#'
#' @export
read_mzml <- function(path, validate = TRUE, lazy = TRUE) {
  MzMlFile(path = path, xml_doc = NULL, validate = validate)
}
