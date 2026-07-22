# Base64 decoding for mzML binary data
# Uses optimized packages when available, with a pure R fallback.

#' Decode base64 string to raw vector
#'
#' Internal function for base64 decoding using available libraries.
#' Tries openssl first, then base64enc, then falls back to pure R.
#'
#' @param x Base64 encoded string
#' @return Raw vector
#' @keywords internal
.base64_decode <- function(x) {
  if (is.null(x) || nchar(x) == 0) {
    return(raw(0))
  }

  # Try openssl first (fastest)
  if (requireNamespace("openssl", quietly = TRUE)) {
    return(openssl::base64_decode(x))
  }

  # Try base64enc
  if (requireNamespace("base64enc", quietly = TRUE)) {
    return(base64enc::base64decode(x))
  }

  # Fall back to pure R implementation
  .base64_decode_pure_r(x)
}

#' Decode base64 string to raw vector using pure R
#'
#' Pure R base64 decoding implementation for when no external packages are available.
#' Optimized for large inputs with pre-allocation.
#'
#' @param x Base64 encoded string
#' @return Raw vector
#' @keywords internal
.base64_decode_pure_r <- function(x) {
  if (is.null(x) || nchar(x) == 0) {
    return(raw(0))
  }

  # Base64 alphabet
  b64_chars <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  b64_vec <- strsplit(b64_chars, "")[[1]]

  # Remove whitespace and count effective length
  x <- gsub("[[:space:]]", "", x)
  n <- nchar(x)

  if (n == 0) {
    return(raw(0))
  }

  # Count padding
  padding <- 0L
  if (substr(x, n, n) == "=") padding <- padding + 1L
  if (substr(x, n - 1, n - 1) == "=") padding <- padding + 1L

  # Convert each character to 6-bit value
  chars <- strsplit(x, "")[[1]]
  values <- match(chars, b64_vec) - 1L
  values[is.na(values)] <- 0L

  # Calculate exact result size
  n_groups <- floor(n / 4)
  remainder <- n %% 4
  result_size <- n_groups * 3 + if (remainder == 2) 1 else if (remainder == 3) 2 else 0 - padding

  if (result_size <= 0) {
    return(raw(0))
  }

  # Pre-allocate result
  result <- raw(result_size)

  idx <- 1L

  # Process complete groups of 4
  for (g in seq_len(n_groups)) {
    v1 <- values[(g - 1) * 4 + 1L]
    v2 <- values[(g - 1) * 4 + 2L]
    v3 <- values[(g - 1) * 4 + 3L]
    v4 <- values[(g - 1) * 4 + 4L]

    # Combine into 3 bytes
    b1 <- bitwOr(bitwShiftL(as.integer(v1), 2), bitwShiftR(as.integer(v2), 4))
    b2 <- bitwOr(bitwShiftL(bitwAnd(as.integer(v2), 15L), 4), bitwShiftR(as.integer(v3), 2))
    b3 <- bitwOr(bitwShiftL(bitwAnd(as.integer(v3), 3), 6), as.integer(v4))

    result[idx] <- as.raw(b1)
    result[idx + 1L] <- as.raw(b2)
    result[idx + 2L] <- as.raw(b3)
    idx <- idx + 3L
  }

  # Handle remaining characters (incomplete group at end)
  if (remainder == 2) {
    v1 <- values[n - 1]
    v2 <- values[n]
    b1 <- bitwOr(bitwShiftL(as.integer(v1), 2), bitwShiftR(as.integer(v2), 4))
    result[idx] <- as.raw(b1)
  } else if (remainder == 3) {
    v1 <- values[n - 2]
    v2 <- values[n - 1]
    v3 <- values[n]
    b1 <- bitwOr(bitwShiftL(as.integer(v1), 2), bitwShiftR(as.integer(v2), 4))
    b2 <- bitwOr(bitwShiftL(bitwAnd(as.integer(v2), 15L), 4), bitwShiftR(as.integer(v3), 2))
    result[idx] <- as.raw(b1)
    result[idx + 1L] <- as.raw(b2)
  }

  result
}
