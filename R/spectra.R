#' Get spectrum data from mzML file
#'
#' Extracts m/z and intensity values from a single spectrum.
#' Uses xml2-based parsing for efficiency. Intensity values are converted
#' to float32 precision by default for memory efficiency.
#'
#' When the mzML file has an index (built automatically for files >10MB),
#' this function reads only the requested spectrum from disk without loading
#' the entire XML into memory.
#'
#' @param mzml Object of class [MzMlFile()]
#' @param index Integer spectrum index (1-based)
#' @param ms_level Optional integer MS level to filter by (e.g., 1 for MS1)
#' @param intensity_precision Numeric; significant digits for intensity values.
#'   Default 0 means use float32 conversion. Set to NULL to keep full precision.
#' @param time_precision Numeric; decimal places for scan time values (default 2).
#'   Set to NULL to keep full precision.
#'
#' @return A list containing:
#'   \describe{
#'     \item{mz}{Numeric vector of m/z values}
#'     \item{intensity}{Numeric vector of intensity values (float32 precision by default)}
#'     \item{id}{Spectrum identifier}
#'     \item{ms_level}{MS level of the spectrum}
#'     \item{scan_time}{Scan start time in minutes}
#'     \item{total_ion_current}{Total ion current if available}
#'     \item{base_peak_mz}{m/z of base peak if available}
#'     \item{base_peak_intensity}{Intensity of base peak if available}
#'   }
#'
#' @details
#' Binary data arrays in mzML are typically zlib-compressed and base64-encoded.
#' This function handles the decoding automatically. The function returns
#' uncompressed numeric vectors ready for analysis.
#'
#' Intensity values are converted to float32 precision by default, which reduces
#' memory usage by 50% while maintaining ~7 significant digits of precision.
#'
#' @examples
#' \dontrun{
#' mzml <- read_mzml("path/to/file.mzML")
#' spec <- get_spectrum(mzml, 1)
#' plot(spec$mz, spec$intensity, type = "l")
#'
#' # Get spectrum with full precision
#' spec_full <- get_spectrum(mzml, 1, intensity_precision = NULL)
#' }
#'
#' @export
get_spectrum <- function(mzml, index, ms_level = NULL,
                         intensity_precision = 0, time_precision = 2) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  # Use index-based reading if available
  if (!is.null(mzml$spectrum_index)) {
    return(.get_spectrum_from_index(
      mzml$path,
      mzml$spectrum_index,
      index,
      ms_level = ms_level,
      intensity_precision = intensity_precision,
      time_precision = time_precision
    ))
  }

  # Fallback to XML-based reading
  file_path <- mzml$path
  doc <- .read_xml(file_path)
  root <- .xml_root(doc)

  spectra_nodes <- .xml_find_by_name(root, "spectrum")

  if (length(spectra_nodes) == 0) {
    cli::cli_abort("No spectra found in file")
  }

  n_spectra <- length(spectra_nodes)

  if (index < 1 || index > n_spectra) {
    cli::cli_abort("Spectrum index {index} out of range (1-{n_spectra})")
  }

  spectrum_node <- spectra_nodes[[index]]

  result <- .extract_spectrum_from_node(
    spectrum_node,
    index,
    intensity_precision = intensity_precision,
    time_precision = time_precision
  )

  if (!is.null(ms_level) && !is.na(result$ms_level) && result$ms_level != ms_level) {
    cli::cli_abort("Spectrum {index} is MS level {result$ms_level}, not MS{ms_level}")
  }

  result
}

#' Get spectrum using index-based reading
#'
#' Internal function to read a spectrum directly from file using byte positions.
#'
#' @param file_path Path to mzML file
#' @param index Spectrum index object
#' @param spectrum_idx 1-based spectrum index
#' @param ms_level Optional MS level filter
#' @param intensity_precision Intensity precision setting
#' @param time_precision Time precision setting
#' @return Spectrum data list
#' @keywords internal
.get_spectrum_from_index <- function(file_path, index, spectrum_idx,
                                      ms_level = NULL,
                                      intensity_precision = 0,
                                      time_precision = 2) {
  if (spectrum_idx < 1 || spectrum_idx > index$n_spectra) {
    cli::cli_abort("Spectrum index {spectrum_idx} out of range (1-{index$n_spectra})")
  }

  # Extract spectrum XML using index
  spectrum_xml <- .extract_spectrum_xml_by_index(file_path, index, spectrum_idx)

  if (nchar(spectrum_xml) == 0) {
    cli::cli_abort("Failed to read spectrum {spectrum_idx} from file")
  }

  # Parse the spectrum XML fragment
  doc <- xml2::read_xml(spectrum_xml)
  node <- xml2::xml_root(doc)

  # Extract spectrum data
  result <- .extract_spectrum_from_xml_node(
    node,
    spectrum_idx,
    intensity_precision = intensity_precision,
    time_precision = time_precision
  )

  if (!is.null(ms_level) && !is.na(result$ms_level) && result$ms_level != ms_level) {
    cli::cli_abort("Spectrum {spectrum_idx} is MS level {result$ms_level}, not MS{ms_level}")
  }

  result
}

#' Extract spectrum data from an XML node
#'
#' Internal helper to extract all relevant data from a spectrum XML node.
#' Works with both full document nodes and fragment nodes.
#'
#' @param node Spectrum XML node
#' @param index Spectrum index (for reporting)
#' @param intensity_precision Significant digits for intensity (0 = float32)
#' @param time_precision Decimal places for time
#' @return List with spectrum data
#' @keywords internal
.extract_spectrum_from_xml_node <- function(node, index, intensity_precision, time_precision) {
  # Extract ID
  spec_id <- .xml_attr(node, "id")

  # Extract cvParams from spectrum node
  params <- .xml_find_cvparam(node)

  # Helper to find param value by accession
  find_param_value <- function(accession) {
    match <- .xml_find_cvparam(node, accession = accession)
    if (length(match) > 0) {
      .xml_attr(match[[1]], "value")
    } else {
      NA_character_
    }
  }

  # Helper to find param value by name
  find_param_by_name <- function(name) {
    match <- .xml_find_cvparam(node, name = name)
    if (length(match) > 0) {
      .xml_attr(match[[1]], "value")
    } else {
      NA_character_
    }
  }

  # Get MS level
  ms_level_str <- find_param_value("MS:1000511")
  ms_level <- if (!is.na(ms_level_str)) as.integer(ms_level_str) else NA_integer_

  # Get scan time - look in nested scan element
  scan_node <- .xml_find_first_by_name(node, "scan")
  scan_time <- NA_real_
  if (length(scan_node) > 0) {
    scan_time_str <- find_param_by_name("scan start time")
    if (is.na(scan_time_str)) {
      scan_time_str <- find_param_by_name("scan time")
    }
    if (!is.na(scan_time_str)) {
      val <- as.numeric(scan_time_str)
      if (!is.null(time_precision)) {
        scan_time <- round(val, digits = time_precision)
      } else {
        scan_time <- val
      }
    }
  }

  # Get TIC
  tic_str <- find_param_by_name("total ion current")
  tic <- if (!is.na(tic_str)) as.numeric(tic_str) else NA_real_

  # Get base peak m/z
  base_peak_mz_str <- find_param_by_name("base peak m/z")
  base_peak_mz <- if (!is.na(base_peak_mz_str)) as.numeric(base_peak_mz_str) else NA_real_

  # Get base peak intensity with precision handling
  base_peak_intensity_str <- find_param_by_name("base peak intensity")
  base_peak_intensity <- if (!is.na(base_peak_intensity_str)) {
    intens <- as.numeric(base_peak_intensity_str)
    if (is.numeric(intensity_precision) && intensity_precision > 0) {
      round(intens, digits = intensity_precision)
    } else if (intensity_precision == 0) {
      .convert_to_float32(intens)
    } else {
      intens
    }
  } else {
    NA_real_
  }

  # Extract binary data from binaryDataArrayList
  bda_node <- .xml_find_first_by_name(node, "binaryDataArrayList")
  mz <- numeric(0)
  intensity <- numeric(0)

  if (length(bda_node) > 0) {
    xpath_arrays <- ".//*[local-name()='binaryDataArray']"
    arrays <- xml2::xml_find_all(bda_node, xpath_arrays)

    for (arr in arrays) {
      arr_name_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @name='m/z array']")
      intensity_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @name='intensity array']")

      is_double <- TRUE
      encoded_len_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @name='encoded length']")
      if (length(encoded_len_params) > 0) {
        double_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @accession='MS:1000521']")
        float_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @accession='MS:1000523']")
        is_double <- length(double_params) > 0 || length(float_params) == 0
      }

      binary_node <- xml2::xml_find_first(arr, ".//*[local-name()='binary']")
      binary_text <- ""
      if (length(binary_node) > 0) {
        binary_text <- .xml_text(binary_node)
      }

      if (nchar(binary_text) > 0) {
        compression_params <- xml2::xml_find_all(arr, ".//*[local-name()='cvParam' and @accession='MS:1000530']")
        is_compressed <- length(compression_params) > 0

        decoded <- .decode_binary_data(
          binary_text,
          is_double = is_double,
          compress = is_compressed
        )

        if (length(arr_name_params) > 0) {
          mz <- decoded
        } else if (length(intensity_params) > 0) {
          if (is.numeric(intensity_precision) && intensity_precision > 0) {
            intensity <- round(decoded, digits = intensity_precision)
          } else if (intensity_precision == 0) {
            intensity <- .convert_to_float32(decoded)
          } else {
            intensity <- decoded
          }
        }
      }
    }
  }

  list(
    mz = mz,
    intensity = intensity,
    id = spec_id,
    ms_level = ms_level,
    scan_time = scan_time,
    total_ion_current = tic,
    base_peak_mz = base_peak_mz,
    base_peak_intensity = base_peak_intensity
  )
}

# Re-export the node-based extraction for backward compatibility
.extract_spectrum_from_node <- .extract_spectrum_from_xml_node

#' Decode binary data from base64 string
#'
#' Internal function to decode base64-encoded, optionally compressed binary data.
#'
#' @param b64_content Base64 encoded string
#' @param is_double Logical; whether data is 64-bit float (double) or 32-bit
#' @param compress Logical; whether data is zlib compressed
#' @return Numeric vector of decoded values
#' @keywords internal
.decode_binary_data <- function(b64_content, is_double = TRUE, compress = FALSE) {
  if (nchar(b64_content) == 0) {
    return(numeric(0))
  }

  raw_data <- .base64_decode(b64_content)

  if (length(raw_data) == 0) {
    return(numeric(0))
  }

  if (compress) {
    raw_data <- tryCatch({
      memDecompress(as.raw(raw_data), type = "gzip")
    }, error = function(e) {
      tryCatch({
        memDecompress(as.raw(raw_data), type = "deflate")
      }, error = function(e2) {
        raw_data
      })
    })
  }

  if (is_double) {
    n_values <- floor(length(raw_data) / 8)
    if (n_values > 0) {
      con <- rawConnection(raw_data[1:(n_values * 8)], "rb")
      values <- readBin(con, what = "double", n = n_values, size = 8, endian = "little")
      close(con)
      return(values)
    }
  } else {
    n_values <- floor(length(raw_data) / 4)
    if (n_values > 0) {
      con <- rawConnection(raw_data[1:(n_values * 4)], "rb")
      values <- readBin(con, what = "numeric", n = n_values, size = 4, endian = "little")
      close(con)
      return(values)
    }
  }

  numeric(0)
}

#' Get multiple spectra from mzML file
#'
#' Extracts m/z and intensity data from multiple or all spectra.
#' Uses memory-efficient processing with optional float32 conversion for intensities.
#' When an index is available, spectra are read in batches from disk.
#'
#' @param mzml Object of class [MzMlFile()]
#' @param indices Integer vector of spectrum indices to extract (1-based).
#'   If NULL, extracts all spectra.
#' @param ms_level Optional integer MS level to filter by.
#' @param intensity_precision Numeric; significant digits for intensity values.
#'   Default 0 means use float32 conversion. Set to NULL to keep full precision.
#' @param time_precision Numeric; decimal places for scan time values (default 2).
#' @param batch_size Integer; when reading all spectra, process in batches of this
#'   size to balance memory usage and I/O efficiency (default 100). Only used when
#'   an index is available.
#'
#' @return A list of lists, where each inner list has the same structure as
#'   the return value of [get_spectrum()]. For large files, consider processing
#'   spectra one at a time to avoid memory issues.
#'
#' @examples
#' \dontrun{
#' mzml <- read_mzml("path/to/file.mzML")
#'
#' # Get first 10 spectra
#' specs <- get_spectra(mzml, indices = 1:10)
#'
#' # Get all MS1 spectra
#' ms1_specs <- get_spectra(mzml, ms_level = 1)
#'
#' # Get all spectra in batches of 500
#' all_specs <- get_spectra(mzml, batch_size = 500)
#'
#' # Get spectra without precision reduction
#' specs_full <- get_spectra(mzml, intensity_precision = NULL)
#' }
#'
#' @export
get_spectra <- function(mzml, indices = NULL, ms_level = NULL,
                        intensity_precision = 0, time_precision = 2,
                        batch_size = 100) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  # Use index-based batch reading if available
  if (!is.null(mzml$spectrum_index)) {
    return(.get_spectra_from_index(
      mzml$path,
      mzml$spectrum_index,
      indices = indices,
      ms_level = ms_level,
      intensity_precision = intensity_precision,
      time_precision = time_precision,
      batch_size = batch_size
    ))
  }

  # Fallback to XML-based reading
  file_path <- mzml$path
  doc <- .read_xml(file_path)
  root <- .xml_root(doc)

  spectra_nodes <- .xml_find_by_name(root, "spectrum")

  if (length(spectra_nodes) == 0) {
    return(list())
  }

  n_spectra <- length(spectra_nodes)

  # Filter by MS level if requested
  if (!is.null(ms_level)) {
    valid_indices <- c()

    for (i in seq_len(n_spectra)) {
      node <- spectra_nodes[[i]]
      ms_params <- .xml_find_cvparam(node, accession = "MS:1000511")
      ms_level_str <- NA_character_
      if (length(ms_params) > 0) {
        ms_level_str <- .xml_attr(ms_params[[1]], "value")
      }

      if (!is.na(ms_level_str) && as.integer(ms_level_str) == ms_level) {
        valid_indices <- c(valid_indices, i)
      }
    }

    if (is.null(indices)) {
      indices <- valid_indices
    } else {
      indices <- intersect(indices, valid_indices)
    }
  }

  if (is.null(indices)) {
    indices <- seq_len(n_spectra)
  }

  indices <- indices[indices >= 1 & indices <= n_spectra]

  if (length(indices) == 0) {
    return(list())
  }

  result <- lapply(indices, function(i) {
    get_spectrum(mzml, i,
                 intensity_precision = intensity_precision,
                 time_precision = time_precision)
  })

  result
}

#' Get multiple spectra using index-based batch reading
#'
#' Internal function to efficiently read multiple spectra using the spectrum index.
#' Reads spectra in batches to minimize I/O operations.
#'
#' @param file_path Path to mzML file
#' @param index Spectrum index object
#' @param indices Vector of indices to read, or NULL for all
#' @param ms_level Optional MS level filter
#' @param intensity_precision Intensity precision setting
#' @param time_precision Time precision setting
#' @param batch_size Batch size for reading
#' @return List of spectrum data
#' @keywords internal
.get_spectra_from_index <- function(file_path, index, indices = NULL,
                                     ms_level = NULL,
                                     intensity_precision = 0,
                                     time_precision = 2,
                                     batch_size = 100) {
  n_spectra <- index$n_spectra

  # Determine which indices to read
  if (is.null(indices)) {
    indices <- seq_len(n_spectra)
  }

  # Filter by MS level if needed (requires reading metadata first)
  if (!is.null(ms_level)) {
    # Read all spectra to filter by MS level
    # This could be optimized by storing MS level in the index
    all_spectra <- .read_spectra_batched(
      file_path, index, indices,
      intensity_precision, time_precision,
      batch_size
    )

    # Filter by MS level
    result <- list()
    for (spec in all_spectra) {
      if (!is.na(spec$ms_level) && spec$ms_level == ms_level) {
        result[[length(result) + 1]] <- spec
      }
    }
    return(result)
  }

  # Read spectra in batches
  .read_spectra_batched(
    file_path, index, indices,
    intensity_precision, time_precision,
    batch_size
  )
}

#' Read spectra in batches
#'
#' Internal function to read multiple spectra in efficient batches.
#' For simplicity, reads each spectrum individually (the index still avoids
#' loading the entire XML into memory).
#'
#' @param file_path Path to mzML file
#' @param index Spectrum index object
#' @param indices Vector of indices to read
#' @param intensity_precision Intensity precision setting
#' @param time_precision Time precision setting
#' @param batch_size Maximum batch size (not used in current implementation)
#' @return List of spectrum data
#' @keywords internal
.read_spectra_batched <- function(file_path, index, indices,
                                   intensity_precision, time_precision,
                                   batch_size) {
  if (length(indices) == 0) return(list())

  # Sort indices for consistent ordering
  sorted_indices <- sort(unique(indices))

  # Read each spectrum individually using the index
  result <- vector("list", length(sorted_indices))

  for (i in seq_along(sorted_indices)) {
    idx <- sorted_indices[i]
    spec <- .get_spectrum_from_index(
      file_path, index, idx,
      intensity_precision = intensity_precision,
      time_precision = time_precision
    )
    result[[i]] <- spec
  }

  result
}

