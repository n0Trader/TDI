#' @title Helper to select column(s) from time-series.
#' @description
#' Helper function to select column(s) from time-series data.
#' @param x Xts time-series with column(s).
#' @param cols List or vector with column(s).
#' @return Time-series with selected column(s).
#' @import xts
#' @export
columns.xts <- function(x, cols) {
  stopifnot(xts::is.xts(x))
  tryCatch(res <- x[, match.String(cols, colnames(x))],
    error = function(e) {
    stop(paste("invalid column(s):", list(cols)), call. = TRUE)
  })
}

#' @title Helper binding column(s) to time-series.
#' @description
#' Helper function to bind `Y` column(s) to `X` time-series data.
#' Be aware that **duplicate columns in `X` get dropped!**
#' @param x Original xts time-series.
#' @param y Xts time-series column(s) to add.
#' @return Xts time-series with selected column(s).
#' @import xts
#' @export
colsbind.xts <- function(x, y) {
  stopifnot(xts::is.xts(x))
  stopifnot(xts::is.xts(y))

  # Mark duplicates to be dropped.
  drop <- which(colnames(x) %in% colnames(y))
  if (length(drop) == length(colnames(x))) {
    # Drop all means we can replace X with Y.
    return(y)
  } else if (length(drop) > 0) {
    # Merge y with x minus dropped column(s).
    return(cbind(x[, -drop], y))
  } else {
    # No duplicates to drop.
    return(cbind(x, y))
  }
}

#' @title Helper extracting OHLC data.
#' @description
#' Helper function to extract the OHCL data from time-series.
#' @param x Time-series with OHCL.
#' @return Time-series with only OHCL.
#' @import xts
#' @export
OHLC.xts <- function(x) {
  stopifnot(xts::is.xts(x))
  return(columns.xts(x, c("Open", "High", "Low", "Close")))
}
