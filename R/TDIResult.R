#' @title TDIResult class
#' @description
#' Base class for all TDI result(s) sub-classes (e.g. Instrument, Index).
#' The class `TDIResult` is the generic class for specific result(s) classes,
#' and implements its shared generic methods.
#' 
#' @docType class
#' @name TDIResult-class
#' @family TDI classes
#' @family TDIResult generics
#' @include TDIObject.R
#' @import xts xts 
#' @export
setClass("TDIResult", contains = c("TDIObject"),
  slots = list(
    #' @slot .source Named list of sources for the data.
    ".sources" = "list", 
    #' @slot .symbol Unique identification symbol.
    ".symbol" = "character", 
    #' @slot .series Time-series of class xts.
    ".series" = "ANY" # Optional S3 class xts
  )
)

#' @title Set slot series
#' @family TDIResult generics
#' @param obj An object of [TDIResult-class].
#' @param df Data frame to be saved.
#' @return Object of class `TDIResult`.
#' @import xts
#' @import zoo
#' @export
setGeneric("setSeries", 
  def = function(obj, df) standardGeneric("setSeries")
)
setMethod("setSeries", signature("TDIResult"), function(obj, df) {
  stopifnot(is.data.frame(df))
  if (nrow(df) > 0) {
    obj@.series <- zoo::na.locf(xts::as.xts(df[,-1], order.by = df$Date))
  }
  invisible(obj)
})

#' @title Get symbol [TDIResult-class]
#' @family TDIResult generics
#' @param obj An object of [TDIResult-class].
#' @return Symbol as character.
#' @export
setGeneric("getSymbol", 
  def = function(obj) standardGeneric("getSymbol")
)
setMethod("getSymbol", signature("TDIResult"), function(obj) {
  return(obj@.symbol)
})

#' @title Get slot series
#' @family TDIResult generics
#' @param obj An object of [TDIResult-class].
#' @return Object series slot.
#' @export
setGeneric("getSeries", 
  def = function(obj) standardGeneric("getSeries")
)
setMethod("getSeries", signature("TDIResult"), function(obj) {
  return(obj@.series)
})

#' @title Add time-series to slot
#' @family TDIResult generics
#' @param obj An object of [TDIResult-class].
#' @param x Time-series to add to series.
#' @return Object TDIResult.
#' @export
setGeneric("addSeries", 
  def = function(obj, x) standardGeneric("addSeries")
)
setMethod("addSeries", signature("TDIResult"), function(obj, x) {
  # Add/update input time-series.
  stopifnot(xts::is.xts(x))
  drop <- which(colnames(obj@.series) %in% colnames(x))
  if (length(drop) > 0) { # prevent duplicate rows
    obj@.series <- obj@.series[, -drop]
  }
  obj@.series <- cbind(obj@.series, x)
  invisible(obj)
})
