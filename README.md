# mzmlr

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R CMD check](https://github.com/RECETOX/mzmlr/actions/workflows/r.yml/badge.svg)](https://github.com/RECETOX/mzmlr/actions/workflows/r.yml)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/mzmlr)](https://CRAN.R-project.org/package=mzmlr) -->
<!-- badges: end -->

## Overview

**mzmlr** is an R package for validating and reading mzML mass spectrometry data files. It provides a lightweight, dependency-minimal alternative to the `mzR` package, implementing native XML parsing capabilities using the `xml2` package.

The package supports:
- Validation against the official HUPO-PSI mzML XSD schema (v1.1.1)
- Reading spectrum data (m/z and intensity arrays)
- Reading chromatogram data (retention time and intensity)
- Extracting metadata (instrument configuration, software, file information)
- Lazy loading for memory-efficient processing of large files

## Installation

### From CRAN

```r
install.packages("mzmlr")
```

### From GitHub

```r
# install.packages("devtools")
devtools::install_github("RECETOX/mzmlr")
```

### Development installation

```bash
git clone https://github.com/RECETOX/mzmlr.git
cd mzmlr
R CMD INSTALL .
```

## Usage

### Basic usage

```r
library(mzmlr)

# Read an mzML file
mzml <- read_mzml("path/to/file.mzML")

# Get file information
info <- get_file_info(mzml)
cat("Spectra:", info$spectrum_count, "\n")
cat("MS levels:", paste(info$ms_levels, collapse = ", "), "\n")

# Get instrument information
instr <- get_instrument_info(mzml)
print(instr)
```

### Reading spectra

```r
# Get a single spectrum
spec <- get_spectrum(mzml, index = 1)
plot(spec$mz, spec$intensity, type = "l", xlab = "m/z", ylab = "Intensity")

# Get multiple spectra
spectra <- get_spectra(mzml, indices = 1:100)

# Get all MS1 spectra
ms1_spectra <- get_spectra(mzml, ms_level = 1)

# Control precision for memory efficiency
# Default (0) uses float32 for intensities
spec_float32 <- get_spectrum(mzml, 1, intensity_precision = 0)
# NULL keeps full double precision
spec_full <- get_spectrum(mzml, 1, intensity_precision = NULL)
```

### Reading chromatograms

```r
# Get all chromatograms
chromatograms <- get_chromatograms(mzml)

# Plot first chromatogram if available
if (length(chromatograms) > 0) {
  plot(chromatograms[[1]]$time, chromatograms[[1]]$intensity,
       type = "l", xlab = "Time", ylab = "Intensity")
}
```

### Reading from URLs

The package supports reading mzML files directly from URLs:

```r
# Read from a URL (file is downloaded temporarily and cleaned up automatically)
mzml <- read_mzml("https://example.com/data/file.mzML")

# With custom timeout for large files
mzml <- read_mzml("https://example.com/data/large_file.mzML", validate = TRUE)
```

### Validation

```r
# Validate an mzML file against the XSD schema
result <- validate_mzml("path/to/file.mzML")

if (result$valid) {
  cat("Valid mzML version", result$version, "\n")
} else {
  cat("Validation errors:", result$message, "\n")
}
```

## Performance Tips

### Memory efficiency

1. **Use lazy loading** (default): Binary data is only decoded when requested
2. **Enable spectrum indexing**: For files >10MB, index is built automatically for efficient random access
3. **Process in batches**: Use `get_spectra()` with indices to process spectra in manageable chunks
4. **Control precision**: Use `intensity_precision = 0` (default) for float32 conversion to reduce memory by 50%

### Speed optimization

1. **Disable validation** if you trust your input files: `read_mzml("file.mzML", validate = FALSE)`
2. **Use batch reading**: Get multiple spectra at once with `get_spectra(mzml, indices = 1:100)`
3. **Filter by MS level**: When possible, use `ms_level` parameter to avoid unnecessary processing

## Features

### MzMlFile class

The core S3 class `MzMlFile` provides a structured representation of mzML files:

```r
mzml <- MzMlFile("path/to/file.mzML", validate = TRUE)
print(mzml)
# <MzMlFile>
#   Path: /path/to/file.mzML
#   Version: 1.1.0
#   Spectra: 4117
```

### Lazy loading and indexing

By default, binary data is not decoded until explicitly requested, enabling efficient handling of large files:

```r
# Only metadata is loaded
mzml <- read_mzml("large_file.mzML", lazy = TRUE)

# Binary data is decoded on demand
spec <- get_spectrum(mzml, 1)
```

### Spectrum indexing for memory-efficient random access

For files larger than 10MB, the package automatically builds a spectrum index that enables memory-efficient random access without loading the entire XML into memory:

```r
# Index is built automatically for files > 10MB
mzml <- read_mzml("large_file.mzML")

# Force index building for smaller files
mzml <- read_mzml("small_file.mzML", build_index = TRUE)

# Disable index building
mzml <- read_mzml("file.mzML", build_index = FALSE)

# Access spectra efficiently using the index
spec <- get_spectrum(mzml, 1)  # Reads only the requested spectrum from disk
```

The index stores byte positions of each spectrum in the file, allowing direct seeking to specific spectra without parsing the entire XML document.

## Dependencies

### Required
- **xml2**: XML parsing
- **cli**: Command line interface and error messages
- **glue**: String interpolation for error messages

### Optional (for improved performance)
- **openssl**: Faster base64 decoding
- **base64enc**: Alternative base64 encoding/decoding

## Development

### Testing

Run tests with:

```r
testthat::test_dir("tests/testthat")
```

### Package checks

```r
devtools::check()
```

## License

MIT + file LICENSE

## References

- HUPO-PSI mzML format: https://www.psidev.info/mzml
- mzML Schema: https://github.com/HUPO-PSI/mzML
- R Packages: https://r-pkgs.org/

## Acknowledgments

This package was developed as a lightweight alternative to `mzR`, specifically designed for research software engineering contexts where minimal dependencies are preferred.
