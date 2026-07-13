# Pure R base64 decoding implementation
# This provides base64 decoding without external dependencies

#' Decode base64 string to raw vector using pure R
#'
#' Internal function for base64 decoding when no external packages are available.
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

  # Remove whitespace and padding
  x <- gsub("[[:space:]]", "", x)

  # Convert each character to 6-bit value
  chars <- strsplit(x, "")[[1]]
  n <- length(chars)

  if (n == 0) {
    return(raw(0))
  }

  # Map characters to values using match (1-indexed, so subtract 1)
  # Need to split b64_chars into individual characters first
  b64_vec <- strsplit(b64_chars, "")[[1]]
  values <- match(chars, b64_vec) - 1L
  values[is.na(values)] <- 0L

  # Combine 6-bit values into 8-bit bytes
  # Group by 4 six-bit values -> 3 eight-bit bytes
  n_groups <- floor(n / 4)
  remainder <- n %% 4

  # Estimate result size
  result_size <- n_groups * 3 + if (remainder == 2) 1 else if (remainder == 3) 2 else 0
  result <- raw(result_size)

  idx <- 1L

  # Process complete groups of 4
  for (g in seq_len(n_groups)) {
    v1 <- values[(g-1)*4 + 1L]
    v2 <- values[(g-1)*4 + 2L]
    v3 <- values[(g-1)*4 + 3L]
    v4 <- values[(g-1)*4 + 4L]

    # Combine into 3 bytes
    b1 <- bitwOr(bitwShiftL(as.integer(v1), 2), bitwShiftR(as.integer(v2), 4))
    b2 <- bitwOr(bitwShiftL(bitwAnd(as.integer(v2), 15), 4), bitwShiftR(as.integer(v3), 2))
    b3 <- bitwOr(bitwShiftL(bitwAnd(as.integer(v3), 3), 6), as.integer(v4))

    result[idx] <- as.raw(b1)
    result[idx+1] <- as.raw(b2)
    result[idx+2] <- as.raw(b3)
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
    b2 <- bitwOr(bitwShiftL(bitwAnd(as.integer(v2), 15), 4), bitwShiftR(as.integer(v3), 2))
    result[idx] <- as.raw(b1)
    result[idx+1] <- as.raw(b2)
  }

  result
}
