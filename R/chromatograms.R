#' Get chromatogram data from mzML file
#'
#' Extracts time/intensity or other chromatographic data from a single chromatogram.
#' Uses xml2-based parsing for efficiency.
#'
#' @param mzml Object of class [MzMlFile()]
#' @param index Integer chromatogram index (1-based)
#' @param time_precision Numeric; decimal places for time values (default 2).
#'   Set to NULL to keep full precision.
#'
#' @return A list containing:
#'   \describe{
#'     \item{time}{Numeric vector of retention times}
#'     \item{intensity}{Numeric vector of intensity values}
#'     \item{id}{Chromatogram identifier}
#'     \item{transition_list}{Transition information if available}
#'     \item{chromatogram_type}{Type of chromatogram (e.g., TIC, BPC)}
#'   }
#'
#' @details
#' Binary data arrays in mzML are typically zlib-compressed and base64-encoded.
#' This function handles the decoding automatically.
#'
#' @examples
#' \dontrun{
#' mzml <- read_mzml("path/to/file.mzML")
#' chrom <- get_chromatogram(mzml, 1)
#' plot(chrom$time, chrom$intensity, type = "l")
#' }
#'
#' @export
get_chromatogram <- function(mzml, index, time_precision = 2) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  # Use the actual file path
  file_path <- mzml$path

  # Read XML document
  doc <- .read_xml(file_path)
  root <- .xml_root(doc)

  # Find all chromatogram elements using local name (namespace-agnostic)
  chrom_nodes <- .xml_find_by_name(root, "chromatogram")

  if (length(chrom_nodes) == 0) {
    cli::cli_abort("No chromatograms found in file")
  }

  n_chroms <- length(chrom_nodes)

  if (index < 1 || index > n_chroms) {
    cli::cli_abort("Chromatogram index {index} out of range (1-{n_chroms})")
  }

  # Get the requested chromatogram node
  chrom_node <- chrom_nodes[[index]]

  # Extract metadata
  chrom_id <- .xml_attr(chrom_node, "id")

  # Find name cvParam for chromatogram type
  name_param <- .xml_find_cvparam(chrom_node, name = "chromatogram type")
  chrom_type <- NA_character_
  if (length(name_param) > 0) {
    chrom_type <- .xml_attr(name_param[[1]], "value")
  }

  # Extract binary data from binaryDataArrayList
  bda_node <- .xml_find_first_by_name(chrom_node, "binaryDataArrayList")
  time_vec <- numeric(0)
  intensity_vec <- numeric(0)

  if (length(bda_node) > 0) {
    # Use descendant search from bda_node
    xpath_arrays <- ".//*[local-name()='binaryDataArray']"
    arrays <- xml2::xml_find_all(bda_node, xpath_arrays)

    for (arr in arrays) {
      # Check what type of array this is using relative XPath
      time_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @name='retention time array']")
      if (length(time_params) == 0) {
        time_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @name='scan time array']")
      }
      intensity_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @name='intensity array']")

      # Extract binary content using relative path
      binary_node <- xml2::xml_find_first(arr, ".//*[local-name()='binary']")
      binary_text <- ""
      if (length(binary_node) > 0) {
        binary_text <- .xml_text(binary_node)
      }

      if (nchar(binary_text) > 0) {
        # Check compression using relative XPath
        compression_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @accession='MS:1000530']")
        is_compressed <- length(compression_params) > 0

        # Time arrays are typically doubles
        decoded <- .decode_binary_data(
          binary_text,
          is_double = TRUE,
          compress = is_compressed
        )

        if (length(time_params) > 0) {
          # Apply time precision rounding
          if (!is.null(time_precision)) {
            time_vec <- round(decoded, digits = time_precision)
          } else {
            time_vec <- decoded
          }
        } else if (length(intensity_params) > 0) {
          intensity_vec <- decoded
        }
      }
    }
  }

  # Get transition list if present
  transition_list <- NULL
  trans_list_node <- .xml_find_first_by_name(chrom_node, "transitionList")
  if (length(trans_list_node) > 0) {
    trans_nodes <- .xml_find_by_name(trans_list_node, "transition")
    if (length(trans_nodes) > 0) {
      transition_list <- lapply(trans_nodes, function(t) {
        prec_mz_params <- .xml_find_cvparam(t, name = "precursor m/z")
        prod_mz_params <- .xml_find_cvparam(t, name = "product m/z")
        list(
          id = .xml_attr(t, "id"),
          precursor_mz = if (length(prec_mz_params) > 0) {
            as.numeric(.xml_attr(prec_mz_params[[1]], "value"))
          } else NA_real_,
          product_mz = if (length(prod_mz_params) > 0) {
            as.numeric(.xml_attr(prod_mz_params[[1]], "value"))
          } else NA_real_
        )
      })
    }
  }

  list(
    time = time_vec,
    intensity = intensity_vec,
    id = chrom_id,
    transition_list = transition_list,
    chromatogram_type = chrom_type
  )
}

#' Get multiple chromatograms from mzML file
#'
#' Extracts chromatographic data from multiple or all chromatograms.
#'
#' @param mzml Object of class [MzMlFile()]
#' @param indices Integer vector of chromatogram indices to extract (1-based).
#'   If NULL, extracts all chromatograms.
#' @param time_precision Numeric; decimal places for time values (default 2).
#'
#' @return A list of lists, where each inner list has the same structure as
#'   the return value of [get_chromatogram()].
#'
#' @examples
#' \dontrun{
#' mzml <- read_mzml("path/to/file.mzML")
#'
#' # Get all chromatograms
#' chrons <- get_chromatograms(mzml)
#'
#' # Plot first chromatogram
#' if (length(chrons) > 0) {
#'   plot(chrons[[1]]$time, chrons[[1]]$intensity, type = "l")
#' }
#' }
#'
#' @export
get_chromatograms <- function(mzml, indices = NULL, time_precision = 2) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  # Use the actual file path
  file_path <- mzml$path

  # Read XML document
  doc <- .read_xml(file_path)
  root <- .xml_root(doc)

  # Find all chromatogram elements using local name (namespace-agnostic)
  chrom_nodes <- .xml_find_by_name(root, "chromatogram")

  if (length(chrom_nodes) == 0) {
    return(list())
  }

  n_chroms <- length(chrom_nodes)

  # Default to all chromatograms
  if (is.null(indices)) {
    indices <- seq_len(n_chroms)
  }

  # Validate indices
  indices <- indices[indices >= 1 & indices <= n_chroms]

  if (length(indices) == 0) {
    return(list())
  }

  # Extract chromatograms
  result <- lapply(indices, function(i) {
    get_chromatogram(mzml, i, time_precision = time_precision)
  })

  result
}
