#' Normalize a Numeric Vector
#'
#' Rescales a numeric vector to the \code{[0, 1]} range using min-max
#' normalization. Typically used to normalize distances before further
#' processing (e.g., plotting or comparison).
#'
#' @param x Numeric vector to normalize.
#'
#' @return A numeric vector of the same length as \code{x}, with values
#'   rescaled to the \code{[0, 1]} range.

# norm distances
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


