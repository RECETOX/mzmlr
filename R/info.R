#' Get file information from mzML object
#'
#' Extracts metadata and summary information from an mzML file.
#'
#' @param mzml Object of class [MzMlFile()]
#'
#' @return A list containing:
#'   \describe{
#'     \item{path}{Absolute path to the file}
#'     \item{version}{mzML schema version}
#'     \item{id}{File identifier}
#'     \item{file_content}{Summary of spectrum/chromatogram types in file}
#'     \item{source_files}{List of source RAW files with checksums}
#'     \item{software}{List of software used for processing}
#'     \item{spectrum_count}{Number of spectra in the run}
#'     \item{chromatogram_count}{Number of chromatograms in the run}
#'     \item{start_time}{Acquisition start time (POSIXct)}
#'     \item{ms_levels}{Unique MS levels present in the file}
#'   }
#'
#' @examples
#' \dontrun{
#' mzml <- read_mzml("path/to/file.mzML")
#' info <- get_file_info(mzml)
#' cat("Spectra:", info$spectrum_count, "\n")
#' cat("MS levels:", paste(info$ms_levels, collapse = ", "), "\n")
#' }
#'
#' @export
get_file_info <- function(mzml) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  root <- .xml_root(mzml$xml)
  ns <- "ns"

  # Read content for regex-based extraction
  content <- .get_xml_content(mzml$xml)

  # Helper function for safe regex matching
  safe_regexec <- function(pattern, text) {
    result <- regexec(pattern, text, ignore.case = TRUE, perl = TRUE)
    if (length(result) == 0 || length(result[[1]]) == 0) return(character(0))
    pos <- result[[1]]
    if (length(pos) > 0 && pos[1] > 0) {
      return(regmatches(text, result)[[1]])
    }
    character(0)
  }

  # File content (types of data present)
  file_content <- list()
  fc_pattern <- '<fileContent[^>]*>(.*?)</fileContent>'
  fc_match <- safe_regexec(fc_pattern, content)
  if (length(fc_match) >= 2) {
    fc_content <- fc_match[2]
    # Extract cvParam entries
    param_pattern <- '<cvParam[^>]*accession=\"([^\"]+)\"[^>]*name=\"([^\"]+)\"'
    params <- gregexpr(param_pattern, fc_content, ignore.case = TRUE, perl = TRUE)
    if (length(params) > 0 && length(params[[1]]) > 0 && params[[1]][1] != -1) {
      matches <- regmatches(fc_content, params)[[1]]
      for (m in matches) {
        acc_match <- safe_regexec('accession="([^"]+)"', m)
        name_match <- safe_regexec('name="([^"]+)"', m)
        if (length(acc_match) >= 2 && length(name_match) >= 2) {
          file_content[[name_match[2]]] <- acc_match[2]
        }
      }
    }
  }

  # Source files
  source_files <- list()
  sf_pattern <- '<sourceFile[^>]*id=\"([^\"]+)\"[^>]*name=\"([^\"]+)\"[^>]*location=\"([^\"]+)\"'
  sf_matches <- gregexpr(sf_pattern, content, ignore.case = TRUE, perl = TRUE)
  if (length(sf_matches) > 0 && length(sf_matches[[1]]) > 0 && sf_matches[[1]][1] != -1) {
    for (m in regmatches(content, sf_matches)[[1]]) {
      id_match <- safe_regexec('id="([^"]+)"', m)
      name_match <- safe_regexec('name="([^"]+)"', m)
      loc_match <- safe_regexec('location="([^"]+)"', m)
      source_files[[length(source_files) + 1]] <- list(
        id = if (length(id_match) >= 2) id_match[2] else NA_character_,
        name = if (length(name_match) >= 2) name_match[2] else NA_character_,
        location = if (length(loc_match) >= 2) loc_match[2] else NA_character_
      )
    }
  }

  # Software list
  software <- list()
  sw_pattern <- '<software[^>]*id=\"([^\"]+)\"[^>]*version=\"([^\"]+)\"'
  sw_matches <- gregexpr(sw_pattern, content, ignore.case = TRUE, perl = TRUE)
  if (length(sw_matches) > 0 && length(sw_matches[[1]]) > 0 && sw_matches[[1]][1] != -1) {
    for (m in regmatches(content, sw_matches)[[1]]) {
      id_match <- safe_regexec('id="([^"]+)"', m)
      ver_match <- safe_regexec('version="([^"]+)"', m)
      software[[length(software) + 1]] <- list(
        id = if (length(id_match) >= 2) id_match[2] else NA_character_,
        version = if (length(ver_match) >= 2) ver_match[2] else NA_character_
      )
    }
  }

  # Run information
  run_pattern <- '<run[^>]*id=\"([^\"]+)\"[^>]*startTimeStamp=\"([^\"]+)\"'
  run_match <- safe_regexec(run_pattern, content)

  spectrum_count <- NA_integer_
  start_time <- NA_character_
  run_id <- NA_character_

  if (length(run_match) >= 3) {
    run_id <- run_match[2]
    start_time <- run_match[3]
  }

  # Get spectrum count from spectrumList
  sl_pattern <- '<spectrumList[^>]*count=\"([^\"]+)\"'
  sl_match <- safe_regexec(sl_pattern, content)
  if (length(sl_match) >= 2 && nchar(sl_match[2]) > 0) {
    spectrum_count <- as.integer(sl_match[2])
  }

  chromatogram_count <- NA_integer_
  chrom_pattern <- '<chromatogramList[^>]*count=\"([^\"]+)\"'
  chrom_match <- safe_regexec(chrom_pattern, content)
  if (length(chrom_match) >= 2 && nchar(chrom_match[2]) > 0) {
    chromatogram_count <- as.integer(chrom_match[2])
  }

  # Get unique MS levels
  ms_levels <- get_ms_levels(mzml)

  list(
    path = mzml$path,
    version = mzml$version,
    id = mzml$id,
    file_content = file_content,
    source_files = source_files,
    software = software,
    spectrum_count = spectrum_count,
    chromatogram_count = chromatogram_count,
    start_time = start_time,
    ms_levels = ms_levels
  )
}

#' Get instrument configuration information
#'
#' Extracts instrument hardware and configuration details from an mzML file.
#'
#' @param mzml Object of class [MzMlFile()]
#'
#' @return A list containing:
#'   \describe{
#'     \item{instrument_id}{Instrument configuration ID}
#'     \item{components}{List of instrument components (source, analyzer, detector)}
#'     \item{serial_number}{Instrument serial number if available}
#'     \item{manufacturer}{Manufacturer information if available}
#'   }
#'
#' @examples
#' \dontrun{
#' mzml <- read_mzml("path/to/file.mzML")
#' instr <- get_instrument_info(mzml)
#' print(instr)
#' }
#'
#' @export
get_instrument_info <- function(mzml) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  content <- .get_xml_content(mzml$xml)

  # Helper function for safe regex matching
  safe_regexec <- function(pattern, text) {
    result <- regexec(pattern, text, ignore.case = TRUE, perl = TRUE)
    if (length(result) == 0 || length(result[[1]]) == 0) return(character(0))
    pos <- result[[1]]
    if (length(pos) > 0 && pos[1] > 0) {
      return(regmatches(text, result)[[1]])
    }
    character(0)
  }

  result <- list(
    instrument_id = NA_character_,
    components = list(),
    serial_number = NA_character_,
    manufacturer = NA_character_
  )

  # Find instrument configuration ID
  instr_pattern <- '<instrumentConfiguration[^>]*id=\"([^\"]+)\"'
  instr_match <- safe_regexec(instr_pattern, content)
  if (length(instr_match) >= 2) {
    result$instrument_id <- instr_match[2]
  }

  # Find serial number
  serial_pattern <- 'name=\"instrument serial number\"[^>]*value=\"([^\"]+)\"'
  serial_match <- safe_regexec(serial_pattern, content)
  if (length(serial_match) >= 2) {
    result$serial_number <- serial_match[2]
  }

  # Find components
  comp_types <- c("source", "analyzer", "detector")
  for (comp_type in comp_types) {
    comp_pattern <- sprintf('<%s[^>]*/?>|<%s[^>]*>', comp_type, comp_type)
    comp_matches <- gregexpr(comp_pattern, content, ignore.case = TRUE)
    if (length(comp_matches) > 0 && length(comp_matches[[1]]) > 0 && comp_matches[[1]][1] != -1) {
      for (comp_tag in regmatches(content, comp_matches)[[1]]) {
        order_match <- safe_regexec('order="([^"]+)"', comp_tag)
        result$components[[length(result$components) + 1]] <- list(
          type = comp_type,
          order = if (length(order_match) >= 2) order_match[2] else NA_character_,
          name = NA_character_
        )
      }
    }
  }

  result
}

#' Get MS levels present in the file
#'
#' Identifies which mass spectrometry levels (MS1, MS2, etc.) are present.
#'
#' @param mzml Object of class [MzMlFile()]
#'
#' @return Integer vector of unique MS levels found
#'
#' @examples
#' \dontrun{
#' mzml <- read_mzml("path/to/file.mzML")
#' levels <- get_ms_levels(mzml)
#' cat("MS levels:", paste(levels, collapse = ", "), "\n")
#' }
#'
#' @export
get_ms_levels <- function(mzml) {
  if (!inherits(mzml, "MzMlFile")) {
    cli::cli_abort("{.arg mzml} must be an MzMlFile object")
  }

  content <- .get_xml_content(mzml$xml)

  # Helper function for safe regex matching
  safe_regexec <- function(pattern, text) {
    result <- regexec(pattern, text, ignore.case = TRUE, perl = TRUE)
    if (length(result) == 0 || length(result[[1]]) == 0) return(character(0))
    pos <- result[[1]]
    if (length(pos) > 0 && pos[1] > 0) {
      return(regmatches(text, result)[[1]])
    }
    character(0)
  }

  # Find all MS level cvParams (accession="MS:1000511")
  pattern <- 'accession=\"MS:1000511\"[^>]*value=\"([^\"]+)\"'
  matches <- gregexpr(pattern, content, ignore.case = TRUE, perl = TRUE)

  if (length(matches) == 0 || length(matches[[1]]) == 0 || matches[[1]][1] == -1) {
    return(integer(0))
  }

  if (length(matches[[1]]) == 0) {
    return(integer(0))
  }
  level_matches <- regmatches(content, matches)[[1]]
  levels <- sapply(level_matches, function(x) {
    val_match <- safe_regexec('value="([^"]+)"', x)
    if (length(val_match) >= 2) {
      as.integer(val_match[2])
    } else {
      NA_integer_
    }
  })

  sort(unique(levels[!is.na(levels)]))
}
