# Spectrum Indexing for Memory-Efficient Access
# This module provides an indexing mechanism that maps spectrum positions
# to byte offsets in the mzML file, enabling lazy loading without keeping
# the entire XML in memory.

#' Build spectrum index from mzML file
#'
#' Scans the mzML file once and records the byte position of each spectrum
#' element. The index allows random access to spectra without loading the
#' entire file into memory. Reads the file line-by-line to minimize memory usage.
#'
#' @param file_path Path to the mzML file
#' @return A list containing:
#'   \describe{
#'     \item{positions}{Integer vector of byte offsets for each spectrum start}
#'     \item{end_positions}{Integer vector of byte offsets for each spectrum end}
#'     \item{n_spectra}{Total number of spectra found}
#'     \item{file_size}{File size in bytes}
#'   }
#' @keywords internal
.build_spectrum_index <- function(file_path) {
  file_size <- file.info(file_path)$size

  if (file_size == 0) {
    return(list(
      positions = integer(0),
      end_positions = integer(0),
      n_spectra = 0L,
      file_size = 0L
    ))
  }

  # Open file connection for reading lines
  con <- file(file_path, "r", encoding = "UTF-8")
  on.exit(close(con))

  # Track byte position
  current_pos <- 0L

  # Pre-allocate vectors (will grow as needed)
  max_estimated_spectra <- floor(file_size / 1000) # Estimate ~1KB per spectrum minimum
  positions <- integer(max(100, max_estimated_spectra))
  end_positions <- integer(max(100, max_estimated_spectra))

  spectrum_count <- 0L
  end_idx <- 1L

  # Pattern for spectrum start tags (not spectrumList or spectrumDescription)
  start_pattern <- "<spectrum(?![a-zA-Z])"
  end_pattern <- "</spectrum>"

  # Read file line by line
  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)

    if (length(line) == 0 || nchar(line) == 0) {
      # Check if we got an empty line at end vs true EOF
      if (current_pos >= file_size) break
      next
    }

    line_bytes <- nchar(line, type = "bytes") + 1L # +1 for newline character

    # Find spectrum start tags in this line
    if (grepl(start_pattern, line, ignore.case = TRUE, perl = TRUE)) {
      # Get all match positions within the line
      matches <- gregexpr(start_pattern, line, ignore.case = TRUE, perl = TRUE)
      if (matches[[1]][1] > 0) {
        for (pos_in_line in as.integer(matches[[1]])) {
          spectrum_count <- spectrum_count + 1L

          # Expand vectors if needed
          if (spectrum_count > length(positions)) {
            new_len <- length(positions) * 2
            positions <- c(positions, integer(new_len))
            end_positions <- c(end_positions, integer(new_len))
          }

          # Calculate absolute byte position
          positions[spectrum_count] <- current_pos + pos_in_line - 1L
        }
      }
    }

    # Find end tags in this line
    if (grepl(end_pattern, line, ignore.case = TRUE)) {
      matches <- gregexpr(end_pattern, line, ignore.case = TRUE)
      if (matches[[1]][1] > 0) {
        match_lengths <- attr(matches[[1]], "match.length")
        for (i in seq_along(matches[[1]])) {
          if (end_idx <= spectrum_count) {
            # Position of end tag + its length
            end_positions[end_idx] <- current_pos + matches[[1]][i] + match_lengths[i] - 1L
            end_idx <- end_idx + 1L
          }
        }
      }
    }

    # Update byte position
    current_pos <- current_pos + line_bytes
  }

  # Trim vectors to actual size
  if (spectrum_count == 0) {
    return(list(
      positions = integer(0),
      end_positions = integer(0),
      n_spectra = 0L,
      file_size = file_size
    ))
  }

  positions <- positions[1:spectrum_count]

  # Fill any missing end positions with NA
  if (end_idx <= spectrum_count) {
    end_positions <- end_positions[1:(end_idx - 1)]
    if (length(end_positions) < spectrum_count) {
      end_positions <- c(end_positions, rep(NA_integer_, spectrum_count - length(end_positions)))
    }
  } else {
    end_positions <- end_positions[1:spectrum_count]
  }

  list(
    positions = positions,
    end_positions = end_positions,
    n_spectra = spectrum_count,
    file_size = file_size
  )
}

#' Extract spectrum XML from file using index
#'
#' Reads a single spectrum's XML content directly from the file using
#' stored byte positions.
#'
#' @param file_path Path to the mzML file
#' @param index Spectrum index object from .build_spectrum_index()
#' @param spectrum_idx 1-based index of spectrum to extract
#' @return Character string containing the spectrum's XML content
#' @keywords internal
.extract_spectrum_xml_by_index <- function(file_path, index, spectrum_idx) {
  if (spectrum_idx < 1 || spectrum_idx > index$n_spectra) {
    cli::cli_abort("Spectrum index {spectrum_idx} out of range (1-{index$n_spectra})")
  }

  # R regex positions are 1-based, but seek() is 0-based
  start_pos <- index$positions[spectrum_idx] - 1L
  end_pos <- index$end_positions[spectrum_idx]

  if (is.na(end_pos) || end_pos <= start_pos + 1) {
    # Fallback: find end position by scanning forward
    con <- file(file_path, "rb")
    on.exit(close(con))

    seek(con, start_pos)
    # Read until we find </spectrum>
    chunk_size <- 8192
    buffer <- ""

    while (TRUE) {
      chunk_raw <- readBin(con, "raw", n = chunk_size)
      if (length(chunk_raw) == 0) break

      chunk_char <- rawToChar(chunk_raw)
      buffer <- paste0(buffer, chunk_char)

      end_match <- regexpr("</spectrum>", buffer, ignore.case = TRUE)
      if (end_match[1] > 0) {
        end_pos <- start_pos + end_match[1] + 10 # Length of </spectrum>
        break
      }

      # Keep last portion to handle split tags
      if (nchar(buffer) > 10000) {
        buffer <- substr(buffer, nchar(buffer) - 5000, nchar(buffer))
      }
    }
  }

  # Read the spectrum content
  con <- file(file_path, "rb")
  on.exit(close(con))

  seek(con, start_pos)
  bytes_to_read <- end_pos - start_pos

  if (bytes_to_read <= 0) {
    return("")
  }

  raw_data <- readBin(con, "raw", n = bytes_to_read)
  rawToChar(raw_data)
}

#' Extract multiple spectra using optimized batch reading
#'
#' Reads multiple spectra from the file, optimizing for sequential access
#' by reading contiguous regions in a single operation.
#'
#' @param file_path Path to the mzML file
#' @param index Spectrum index object
#' @param indices Vector of 1-based spectrum indices to extract
#' @return Named list of XML strings, named by spectrum index
#' @keywords internal
.extract_spectra_batch <- function(file_path, index, indices) {
  if (length(indices) == 0) {
    return(list())
  }

  # Sort indices for efficient sequential reading
  sorted_indices <- sort(indices)

  # Group consecutive indices into batches
  batches <- .group_consecutive_indices(sorted_indices)

  result <- list()

  for (batch in batches) {
    first_idx <- batch[1]
    last_idx <- batch[length(batch)]

    start_pos <- index$positions[first_idx]
    end_pos <- index$end_positions[last_idx]

    # Handle missing end position
    if (is.na(end_pos)) {
      end_pos <- index$positions[last_idx]
      # Extend to include the full last spectrum
      con <- file(file_path, "rb")
      on.exit(close(con), add = TRUE)
      seek(con, end_pos)
      chunk_raw <- readBin(con, "raw", n = 65536) # Read up to 64KB
      chunk_char <- rawToChar(chunk_raw)
      end_match <- regexpr("</spectrum>", chunk_char, ignore.case = TRUE)
      if (end_match[1] > 0) {
        end_pos <- end_pos + end_match[1] + 10
      }
    }

    # Read the batch region
    con <- file(file_path, "rb")
    seek(con, start_pos)
    bytes_to_read <- end_pos - start_pos

    if (bytes_to_read > 0) {
      raw_data <- readBin(con, "raw", n = bytes_to_read)
      batch_xml <- rawToChar(raw_data)

      # Parse individual spectra from batch
      spec_pattern <- "<spectrum(?!List)[^>]*>"
      spec_matches <- gregexpr(spec_pattern, batch_xml, ignore.case = TRUE, perl = TRUE)

      if (spec_matches[[1]][1] > 0) {
        starts <- spec_matches[[1]]
        lengths <- attr(spec_matches[[1]], "match.length")

        for (i in seq_along(starts)) {
          # Find corresponding end tag
          remaining <- substr(batch_xml, starts[i], nchar(batch_xml))
          end_match <- regexpr("</spectrum>", remaining, ignore.case = TRUE)
          if (end_match[1] > 0) {
            spec_xml <- substr(remaining, 1, end_match[1] + 10)

            # Determine which spectrum index this is
            # Use relative position within batch
            rel_pos <- starts[i] - 1
            for (j in batch) {
              if (index$positions[j] - start_pos == rel_pos) {
                result[[as.character(j)]] <- spec_xml
                break
              }
            }
          }
        }
      }
    }
    close(con)
  }

  result
}

#' Group consecutive indices for efficient batch reading
#'
#' Groups a sorted vector of indices into runs of consecutive values.
#' Gaps larger than threshold start a new batch.
#'
#' @param indices Sorted integer vector of indices
#' @param max_gap Maximum gap between indices to be considered consecutive (default 100)
#' @return List of integer vectors, each representing a batch
#' @keywords internal
.group_consecutive_indices <- function(indices, max_gap = 100) {
  if (length(indices) == 0) {
    return(list())
  }

  batches <- list()
  current_batch <- indices[1]

  for (i in 2:length(indices)) {
    if (indices[i] - indices[i - 1] <= max_gap) {
      current_batch <- c(current_batch, indices[i])
    } else {
      batches[[length(batches) + 1]] <- current_batch
      current_batch <- indices[i]
    }
  }

  batches[[length(batches) + 1]] <- current_batch
  batches
}
