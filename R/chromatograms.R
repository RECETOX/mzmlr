#' Get chromatogram data from mzML file
#'
#' Extracts time/intensity or other chromatographic data from a single chromatogram.
#'
#' @param mzml Object of class [MzMlFile()]
#' @param index Integer chromatogram index (1-based)
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
#' @examples
#' \dontrun{
#' mzml <- read_mzml("path/to/file.mzML")
#' chrom <- get_chromatogram(mzml, 1)
#' plot(chrom$time, chrom$intensity, type = "l")
#' }
#'
#' @export
get_chromatogram <- function(mzml, index) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  content <- .get_xml_content(mzml$xml)

  # Find chromatogram list
  if (!grepl("<chromatogramList", content, ignore.case = TRUE)) {
    cli::cli_abort("No chromatograms found in file")
  }

  # Find all chromatogram tags
  chrom_pattern <- '<chromatogram[^>]*/?>|<chromatogram[^>]*>'
  chrom_matches <- gregexpr(chrom_pattern, content, ignore.case = TRUE)

  if (chrom_matches[[1]][1] == -1) {
    cli::cli_abort("No chromatograms found in file")
  }

  all_positions <- chrom_matches[[1]]
  n_chroms <- length(all_positions[all_positions > 0])

  if (index < 1 || index > n_chroms) {
    cli::cli_abort("Chromatogram index {index} out of range (1-{n_chroms})")
  }

  # Extract this chromatogram's content
  start_pos <- all_positions[index]

  if (index < n_chroms) {
    end_pos <- all_positions[index + 1] - 1
  } else {
    close_pattern <- '</chromatogram>'
    close_match <- regexpr(close_pattern, substr(content, start_pos + 100, nchar(content)), ignore.case = TRUE)
    end_pos <- start_pos + close_match[1] + 12
  }

  chrom_content <- substr(content, start_pos, end_pos)

  # Extract metadata
  id_match <- regmatches(chrom_content, regexec('id=\"([^\"]+)\"', chrom_content))[[1]]
  chrom_id <- if (length(id_match) >= 2) id_match[2] else NA_character_

  # Get chromatogram type
  type_pattern <- 'name=\"([^\"]*chromatogram[^\"]*)\"[^>]*accession=\"([^\"]+)\"'
  type_match <- regmatches(chrom_content, regexec(type_pattern, chrom_content, ignore.case = TRUE, perl = TRUE))[[1]]
  chrom_type <- if (length(type_match) >= 3) type_match[2] else NA_character_

  # Extract binary data
  bda_pattern <- '<binaryDataArrayList[^>]*>(.*?)</binaryDataArrayList>'
  bda_match <- regmatches(chrom_content, regexec(bda_pattern, chrom_content, ignore.case = TRUE, perl = TRUE))[[1]]

  time_vec <- numeric(0)
  intensity_vec <- numeric(0)

  if (length(bda_match) >= 2) {
    bda_content <- bda_match[2]

    # Find arrays
    array_pattern <- '<binaryDataArray[^>]*>(.*?)</binaryDataArray>'
    arrays <- gregexpr(array_pattern, bda_content, ignore.case = TRUE, perl = TRUE)

    if (arrays[[1]][1] != -1) {
      for (arr in regmatches(bda_content, arrays)[[1]]) {
        if (grepl('name=\"retention time array\"|name=\"scan time array\"', arr, ignore.case = TRUE)) {
          time_vec <- .decode_binary_from_tag(arr)
        } else if (grepl('name=\"intensity array\"', arr, ignore.case = TRUE)) {
          intensity_vec <- .decode_binary_from_tag(arr)
        }
      }
    }
  }

  # Get transition list if present
  transition_list <- NULL
  if (grepl("<transitionList", chrom_content, ignore.case = TRUE)) {
    trans_pattern <- '<transition[^>]*id=\"([^\"]+)\"'
    trans_matches <- gregexpr(trans_pattern, chrom_content, ignore.case = TRUE)
    if (trans_matches[[1]][1] != -1) {
      transitions <- regmatches(chrom_content, trans_matches)[[1]]
      transition_list <- lapply(transitions, function(t) {
        list(
          id = regmatches(t, regexec('id=\"([^\"]+)\"', t))[[1]][2]
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
get_chromatograms <- function(mzml, indices = NULL) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  content <- .get_xml_content(mzml$xml)

  # Check for chromatogram list
  if (!grepl("<chromatogramList", content, ignore.case = TRUE)) {
    return(list())
  }

  # Find all chromatogram positions
  chrom_pattern <- '<chromatogram[^>]*/?>|<chromatogram[^>]*>'
  chrom_matches <- gregexpr(chrom_pattern, content, ignore.case = TRUE)

  if (chrom_matches[[1]][1] == -1) {
    return(list())
  }

  all_positions <- chrom_matches[[1]]
  n_chroms <- length(all_positions[all_positions > 0])

  if (n_chroms == 0) {
    return(list())
  }

  # Default to all chromatograms
  if (is.null(indices)) {
    indices <- seq_len(n_chroms)
  }

  # Validate indices
  indices <- indices[indices >= 1 & indices <= n_chroms]

  # Extract chromatograms
  result <- lapply(indices, function(i) {
    get_chromatogram(mzml, i)
  })

  result
}
