#' Get spectrum data from mzML file
#'
#' Extracts m/z and intensity values from a single spectrum.
#'
#' @param mzml Object of class [MzMlFile()]
#' @param index Integer spectrum index (1-based)
#' @param ms_level Optional integer MS level to filter by (e.g., 1 for MS1)
#'
#' @return A list containing:
#'   \describe{
#'     \item{mz}{Numeric vector of m/z values}
#'     \item{intensity}{Numeric vector of intensity values}
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
#' @examples
#' \dontrun{
#' mzml <- read_mzml("path/to/file.mzML")
#' spec <- get_spectrum(mzml, 1)
#' plot(spec$mz, spec$intensity, type = "l")
#' }
#'
#' @export
get_spectrum <- function(mzml, index, ms_level = NULL) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  content <- .get_xml_content(mzml$xml)

  # Find all spectrum tags (excluding spectrumList)
  spectrum_pattern <- '<spectrum(?!List)[^>]*/?>|<spectrum(?!List)[^>]*>'
  spectrum_matches <- gregexpr(spectrum_pattern, content, ignore.case = TRUE, perl = TRUE)

  if (length(spectrum_matches) == 0 || length(spectrum_matches[[1]]) == 0 || spectrum_matches[[1]][1] == -1) {
    cli::cli_abort("No spectra found in file")
  }

  spectrum_positions <- if (length(spectrum_matches) > 0 && length(spectrum_matches[[1]]) > 0) spectrum_matches[[1]] else integer(0)
  n_spectra <- length(spectrum_positions[spectrum_positions > 0])

  if (index < 1 || index > n_spectra) {
    cli::cli_abort("Spectrum index {index} out of range (1-{n_spectra})")
  }

  # Extract this spectrum's content
  start_pos <- spectrum_positions[index]

  # Find the end of this spectrum (next spectrum or closing tag)
  if (index < n_spectra) {
    end_pos <- spectrum_positions[index + 1] - 1
  } else {
    # Find </spectrum> after this position
    close_pattern <- '</spectrum>'
    close_match <- regexpr(close_pattern, substr(content, start_pos + 100, nchar(content)), ignore.case = TRUE)
    end_pos <- start_pos + close_match[1] + 9
  }

  spectrum_content <- substr(content, start_pos, end_pos)

  # Extract spectrum metadata
  id_match <- regmatches(spectrum_content, regexec('id="([^"]+)"', spectrum_content))[[1]]
  spec_id <- if (length(id_match) >= 2) id_match[2] else NA_character_

  default_len_match <- regmatches(spectrum_content, regexec('defaultArrayLength="([^"]+)"', spectrum_content))[[1]]
  default_array_len <- if (length(default_len_match) >= 2) as.integer(default_len_match[2]) else NA_integer_

  # Get MS level
  ms_pattern <- 'accession="MS:1000511"[^>]*value="([^"]+)"'
  ms_match <- regmatches(spectrum_content, regexec(ms_pattern, spectrum_content, ignore.case = TRUE, perl = TRUE))[[1]]
  ms_level_val <- if (length(ms_match) >= 2) as.integer(ms_match[2]) else NA_integer_

  # Check MS level filter
  if (!is.null(ms_level) && ms_level_val != ms_level) {
    cli::cli_abort("Spectrum {index} is MS level {ms_level_val}, not MS{ms_level}")
  }

  # Helper to extract value by name (handles different attribute orders)
  extract_param_value <- function(content, param_name) {
    # Try name first, then value-first pattern
    patterns <- c(
      sprintf('name="%s"[^>]*value="([^"]+)"', param_name),
      sprintf('value="([^"]+)"[^/>]*name="%s"', param_name)
    )

    for (pattern in patterns) {
      match <- regmatches(content, regexec(pattern, content, ignore.case = TRUE, perl = TRUE))[[1]]
      if (length(match) >= 2 && nchar(match[2]) > 0) {
        return(as.numeric(match[2]))
      }
    }
    NA_real_
  }

  # Get scan time
  scan_time <- extract_param_value(spectrum_content, "scan start time")

  # Get TIC
  tic <- extract_param_value(spectrum_content, "total ion current")

  # Get base peak m/z
  base_peak_mz <- extract_param_value(spectrum_content, "base peak m/z")

  # Get base peak intensity
  base_peak_intensity <- extract_param_value(spectrum_content, "base peak intensity")

  # Extract binary data
  mz <- numeric(0)
  intensity <- numeric(0)

  # Find binaryDataArrayList section
  bda_start <- regexpr('<binaryDataArrayList', spectrum_content, ignore.case = TRUE)
  if (bda_start[1] > 0) {
    bda_end <- regexpr('</binaryDataArrayList>', substr(spectrum_content, bda_start[1], nchar(spectrum_content)), ignore.case = TRUE)
    if (bda_end[1] > 0) {
      bda_content <- substr(spectrum_content, bda_start[1], bda_start[1] + bda_end[1] + 19)

      # Find all binaryDataArray elements by position
      arr_pattern <- '<binaryDataArray[^>]*/?>|<binaryDataArray[^>]*>'
      arr_matches <- gregexpr(arr_pattern, bda_content, ignore.case = TRUE)

      if (arr_matches[[1]][1] > 0) {
        arr_positions <- arr_matches[[1]]

        for (i in seq_along(arr_positions)) {
          arr_start <- arr_positions[i]

          # Find end of this array
          if (i < length(arr_positions)) {
            arr_end <- arr_positions[i + 1] - 1
          } else {
            close_pos <- regexpr('</binaryDataArray>', substr(bda_content, arr_start, nchar(bda_content)), ignore.case = TRUE)
            arr_end <- arr_start + close_pos[1] + 15
          }

          arr_content <- substr(bda_content, arr_start, min(arr_end, nchar(bda_content)))

          # Check if this is m/z or intensity array
          if (grepl('name="m/z array"', arr_content, ignore.case = TRUE)) {
            mz <- .decode_binary_from_tag(arr_content)
          } else if (grepl('name="intensity array"', arr_content, ignore.case = TRUE)) {
            intensity <- .decode_binary_from_tag(arr_content)
          }
        }
      }
    }
  }

  list(
    mz = mz,
    intensity = intensity,
    id = spec_id,
    ms_level = ms_level_val,
    scan_time = scan_time,
    total_ion_current = tic,
    base_peak_mz = base_peak_mz,
    base_peak_intensity = base_peak_intensity
  )
}

# Internal function to decode binary data from a tag
.decode_binary_from_tag <- function(tag) {
  # Check compression type
  is_compressed <- grepl('compression', tag, ignore.case = TRUE) &&
                   !grepl('no compression', tag, ignore.case = TRUE)

  # Check data type
  is_double <- grepl('64-bit float|double', tag, ignore.case = TRUE)

  # Extract base64 content
  binary_pattern <- '<binary[^>]*>(.+?)</binary>'
  binary_match <- regmatches(tag, regexec(binary_pattern, tag, ignore.case = TRUE, perl = TRUE))[[1]]

  if (length(binary_match) < 2 || nchar(binary_match[2]) == 0) {
    return(numeric(0))
  }

  b64_content <- binary_match[2]

  # Decode base64
  raw_data <- tryCatch({
    if (requireNamespace("openssl", quietly = TRUE)) {
      openssl::base64_decode(b64_content)
    } else if (requireNamespace("base64enc", quietly = TRUE)) {
      base64enc::base64decode(b64_content)
    } else {
      # Use pure R base64 decoder
      .base64_decode_pure_r(b64_content)
    }
  }, error = function(e) {
    raw(0)
  })

  if (length(raw_data) == 0) {
    return(numeric(0))
  }

  # Decompress if needed using memDecompress
  if (is_compressed) {
    raw_data <- tryCatch({
      # Try gzip first (most common for mzML)
      memDecompress(as.raw(raw_data), type = "gzip")
    }, error = function(e) {
      # Fall back to no decompression
      raw_data
    })
  }

  # Convert to numeric based on data type
  if (is_double) {
    # 64-bit double (8 bytes)
    n_values <- floor(length(raw_data) / 8)
    if (n_values > 0) {
      con <- rawConnection(raw_data[1:(n_values * 8)], "rb")
      values <- readBin(con, what = "double", n = n_values, size = 8, endian = "little")
      close(con)
      return(values)
    }
  } else {
    # 32-bit float (4 bytes) - stored as numeric in R
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
#'
#' @param mzml Object of class [MzMlFile()]
#' @param indices Integer vector of spectrum indices to extract (1-based).
#'   If NULL, extracts all spectra.
#' @param ms_level Optional integer MS level to filter by.
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
#' }
#'
#' @export
get_spectra <- function(mzml, indices = NULL, ms_level = NULL) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  content <- .get_xml_content(mzml$xml)

  # Find all spectrum positions
  spectrum_pattern <- '<spectrum(?!List)[^>]*/?>|<spectrum(?!List)[^>]*>'
  spectrum_matches <- gregexpr(spectrum_pattern, content, ignore.case = TRUE, perl = TRUE)

  if (length(spectrum_matches) == 0 || length(spectrum_matches[[1]]) == 0 || spectrum_matches[[1]][1] == -1) {
    return(list())
  }

  all_positions <- if (length(spectrum_matches) > 0 && length(spectrum_matches[[1]]) > 0) spectrum_matches[[1]] else integer(0)
  n_spectra <- length(all_positions[all_positions > 0])

  # Filter by MS level if requested
  if (!is.null(ms_level)) {
    valid_indices <- c()
    for (i in seq_len(n_spectra)) {
      start_pos <- all_positions[i]
      # Find end
      if (i < n_spectra) {
        end_pos <- all_positions[i + 1] - 1
      } else {
        close_pattern <- '</spectrum>'
        close_match <- regexpr(close_pattern, substr(content, start_pos + 100, nchar(content)), ignore.case = TRUE)
        end_pos <- start_pos + close_match[1] + 9
      }
      spec_content <- substr(content, start_pos, end_pos)

      # Check MS level
      ms_pattern <- 'accession="MS:1000511"[^>]*value="([^"]+)"'
      ms_match <- regmatches(spec_content, regexec(ms_pattern, spec_content, ignore.case = TRUE, perl = TRUE))[[1]]
      if (length(ms_match) >= 2) {
        file_ms_level <- as.integer(ms_match[2])
        if (file_ms_level == ms_level) {
          valid_indices <- c(valid_indices, i)
        }
      }
    }
    if (is.null(indices)) {
      indices <- valid_indices
    } else {
      indices <- intersect(indices, valid_indices)
    }
  }

  # Default to all spectra
  if (is.null(indices)) {
    indices <- seq_len(n_spectra)
  }

  # Validate indices
  indices <- indices[indices >= 1 & indices <= n_spectra]

  # Extract spectra
  result <- lapply(indices, function(i) {
    get_spectrum(mzml, i)
  })

  result
}
