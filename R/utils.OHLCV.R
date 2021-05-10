# Implementation of generic OHLCV functions.
# With credits to the Quantmod developers.

#' @title Get Open price(s).
#' @description 
#' Retrieve the Open price(s) from the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @return Open price(s).
#' @export
Op <- function(x) {
  open <- has.Op(x, which = TRUE)
  if (!identical(open, integer(0))) return(x[, open])
  stop('Subscript out of bounds: Open prices not found.')
}

#' @title Check for Open price(s).
#' @description 
#' Check if Open price(s) exists in the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @param which Boolean to ask for the column number.
#' @return Boolean or column number.
#' @export
has.Op <- function(x, which = FALSE) {
  n <- grep("Open$", colnames(x), ignore.case = TRUE)
  return(if(which) n else !identical(n,integer(0)))
}

#' @title Get High price(s).
#' @description 
#' Retrieve the High price(s) from the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @return High price(s).
#' @export
Hi <- function(x) {
  high <- has.Hi(x, which = TRUE)
  if (!identical(high, integer(0))) return(x[, high])
  stop('Subscript out of bounds: High prices not found.')
}

#' @title Check for High price(s).
#' @description 
#' Check if High price(s) exists in the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @param which Boolean to ask for the column number.
#' @return Boolean or column number.
#' @export
has.Hi <- function(x, which = FALSE) {
  n <- grep("High$", colnames(x), ignore.case = TRUE)
  return(if(which) n else !identical(n,integer(0)))
}

#' @title Get Low price(s).
#' @description 
#' Retrieve the Low price(s) from the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @return Low price(s).
#' @export
Lo <- function(x) {
  low <- has.Lo(x, which = TRUE)
  if (!identical(low, integer(0))) return(x[, low])
  stop('Subscript out of bounds: Low prices not found.')
}

#' @title Check for Low price(s).
#' @description 
#' Check if Low price(s) exists in the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @param which Boolean to ask for the column number.
#' @return Boolean or column number.
#' @export
has.Lo <- function(x, which = FALSE) {
  n <- grep("Low$", colnames(x), ignore.case = TRUE)
  return(if(which) n else !identical(n,integer(0)))
}

#' @title Get Close price(s).
#' @description 
#' Retrieve the Close price(s) from the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @return Close price(s).
#' @export
Cl <- function(x) {
  close <- has.Cl(x, which = TRUE)
  if (!identical(close, integer(0))) return(x[, close])
  stop('Subscript out of bounds: Close prices not found.')
}

#' @title Check for Close price(s).
#' @description 
#' Check if Close price(s) exists in the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @param which Boolean to ask for the column number.
#' @return Boolean or column number.
#' @export
has.Cl <- function(x, which = FALSE) {
  n <- grep("Close$", colnames(x), ignore.case = TRUE)
  return(if(which) n else !identical(n,integer(0)))
}

#' @title Get trading Volume(s).
#' @description 
#' Retrieve the trading Volume(s) from the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @return Volume(s) traded.
#' @export
Vo <- function(x) {
  vol <- has.Vo(x, which = TRUE)
  if (!identical(vol, integer(0))) return(x[, vol])
  stop('Subscript out of bounds: Volumes not found.')
}

#' @title Check for trading Volume(s).
#' @description 
#' Check if trading Volume(s) exists in the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @param which Boolean to ask for the column number.
#' @return Boolean or column number.
#' @export
has.Vo <- function(x, which = FALSE) {
  n <- grep("Volume$", colnames(x), ignore.case = TRUE)
  return(if(which) n else !identical(n,integer(0)))
}

#' @title Get OHLC price(s).
#' @description 
#' Retrieve OHLC price(s) from the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @return OHLC price(s).
#' @export
OHLC <- function(x) {
  if (has.OHLC(x)) return(x[, has.OHLC(x, which = TRUE)])
  stop('Subscript out of bounds: OHCL not found.')
}

#' @title Check for OHLC price(s).
#' @description 
#' Check if OHLC exists in the data series.
#' @param x Matrix or time-series data with multiple columns.
#' @param which Boolean to ask for the column number.
#' @param all Boolean to ask all at once.
#' @return Boolean or column number(s).
#' @export
has.OHLC <- function(x, which = FALSE, all = TRUE) {
  if (which) { return(c(has.Op(x, TRUE), has.Hi(x, TRUE), has.Lo(x, TRUE), has.Cl(x, TRUE)))
  } else if (all) { return(all(has.Op(x), has.Hi(x), has.Lo(x), has.Cl(x)))
  } else return(c(has.Op(x), has.Hi(x), has.Lo(x), has.Cl(x)))
}
