#' Get file information from mzML object
#'
#' Extracts metadata and summary information from an mzML file using xml2-based
#' parsing for efficiency.
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

  # File content (types of data present)
  file_content <- list()
  fc_node <- .xml_find_first_by_name(root, "fileContent")
  if (length(fc_node) > 0) {
    cv_params <- .xml_find_cvparam(fc_node)
    for (param in cv_params) {
      name <- .xml_attr(param, "name")
      accession <- .xml_attr(param, "accession")
      if (!is.na(name) && !is.na(accession)) {
        file_content[[name]] <- accession
      }
    }
  }

  # Source files
  source_files <- list()
  sf_nodes <- .xml_find_by_name(root, "sourceFile")
  for (sf in sf_nodes) {
    id <- .xml_attr(sf, "id")
    name <- .xml_attr(sf, "name")
    location <- .xml_attr(sf, "location")

    # Get checksum if available
    checksum_param <- .xml_find_cvparam(sf, name = "local checksum")
    checksum <- NA_character_
    if (length(checksum_param) > 0) {
      checksum <- .xml_attr(checksum_param[[1]], "value")
    }

    source_files[[length(source_files) + 1]] <- list(
      id = if (!is.na(id)) id else NA_character_,
      name = if (!is.na(name)) name else NA_character_,
      location = if (!is.na(location)) location else NA_character_,
      checksum = checksum
    )
  }

  # Software list
  software <- list()
  sw_nodes <- .xml_find_by_name(root, "software")
  for (sw in sw_nodes) {
    id <- .xml_attr(sw, "id")
    version <- .xml_attr(sw, "version")

    software[[length(software) + 1]] <- list(
      id = if (!is.na(id)) id else NA_character_,
      version = if (!is.na(version)) version else NA_character_
    )
  }

  # Run information
  run_node <- .xml_find_first_by_name(root, "run")
  run_id <- NA_character_
  start_time <- NA_character_

  if (length(run_node) > 0) {
    run_id <- .xml_attr(run_node, "id")
    start_time <- .xml_attr(run_node, "startTimeStamp")
  }

  # Get spectrum count from spectrumList
  spectrum_count <- NA_integer_
  sl_node <- .xml_find_first_by_name(root, "spectrumList")
  if (length(sl_node) > 0) {
    count_str <- .xml_attr(sl_node, "count")
    if (!is.na(count_str) && count_str != "") {
      spectrum_count <- as.integer(count_str)
    }
  }

  # Get chromatogram count
  chromatogram_count <- NA_integer_
  cl_node <- .xml_find_first_by_name(root, "chromatogramList")
  if (length(cl_node) > 0) {
    count_str <- .xml_attr(cl_node, "count")
    if (!is.na(count_str) && count_str != "") {
      chromatogram_count <- as.integer(count_str)
    }
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

  root <- .xml_root(mzml$xml)

  result <- list(
    instrument_id = NA_character_,
    components = list(),
    serial_number = NA_character_,
    manufacturer = NA_character_
  )

  # Find instrument configuration ID
  instr_node <- .xml_find_first_by_name(root, "instrumentConfiguration")
  if (length(instr_node) > 0) {
    result$instrument_id <- .xml_attr(instr_node, "id")
  }

  # Find serial number
  serial_param <- .xml_find_cvparam(root, name = "instrument serial number")
  if (length(serial_param) > 0) {
    result$serial_number <- .xml_attr(serial_param[[1]], "value")
  }

  # Find manufacturer
  manufacturer_param <- .xml_find_cvparam(root, name = "manufacturer")
  if (length(manufacturer_param) > 0) {
    result$manufacturer <- .xml_attr(manufacturer_param[[1]], "value")
  }

  # Find components
  comp_types <- c("source", "analyzer", "detector")
  for (comp_type in comp_types) {
    comp_nodes <- .xml_find_by_name(root, comp_type)
    for (comp in comp_nodes) {
      order_attr <- .xml_attr(comp, "order")
      result$components[[length(result$components) + 1]] <- list(
        type = comp_type,
        order = if (!is.na(order_attr)) order_attr else NA_character_,
        name = NA_character_
      )
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

  root <- .xml_root(mzml$xml)

  # Find all MS level cvParams (accession="MS:1000511")
  ms_params <- .xml_find_cvparam(root, accession = "MS:1000511")

  if (length(ms_params) == 0) {
    return(integer(0))
  }

  levels <- sapply(ms_params, function(p) {
    val <- .xml_attr(p, "value")
    if (!is.na(val) && val != "") {
      as.integer(val)
    } else {
      NA_integer_
    }
  })

  sort(unique(levels[!is.na(levels)]))
}
