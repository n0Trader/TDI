#' @title TDIResult class
#' @description
#' Base class for all TDI result(s) sub-classes (e.g. Instrument, Index).
#' The class `TDIResult` is the generic class for specific result(s) classes,
#' and implements its shared generic methods.
#' @docType class
#' @name TDIResult-class
#' @family TDI classes
#' @family TDIResult generics
#' @include TDIObject.R
#' @import xts xts 
#' @export
setClass("TDIResult", contains = c("TDIObject"),
  slots = list(
    #' @slot .sources Named list of sources for the data.
    ".sources" = "list", 
    #' @slot .symbol Unique identification symbol.
    ".symbol" = "character", 
    #' @slot .series Time-series of class xts.
    ".series" = "ANY" # Optional S3 class xts
  )
)

#' @title Set series for `TDIResult`
#' @docType methods
#' @family TDIResult generics
#' @param obj An object of class `TDIResult`.
#' @param x Data for slot series.
#' @return Object of class `TDIResult` with series.
#' @import xts
#' @import zoo
#' @export
setGeneric("setSeries", 
  def = function(obj, x) standardGeneric("setSeries")
)
#' @rdname setSeries
setMethod("setSeries", signature("TDIResult"), function(obj, x) {
  # Check if there is existing data.
  if (length(obj@.series) > 0) {
    stopifnot(xts::is.xts(x))
    
    # Mark duplicates to be dropped.
    drop <- which(colnames(obj@.series) %in% colnames(x))
    if (length(drop) == length(colnames(obj@.series))) {
      # Drop all means we can replace X with Y.
      obj@.series <- x
    } else if (length(drop) > 0) {
      # Merge y with x minus dropped column(s).
      obj@.series <- cbind(obj@.series[, -drop], x)
    } else {
      # No duplicates to drop.
      obj@.series <- cbind(obj@.series, x)
    }
    
  } else if (is.data.frame(x)) {
    obj@.series <- zoo::na.locf(xts::as.xts(x[,-1], order.by = x$Date))
  } else if (xts::is.xts(x)) {
    obj@.series <- zoo::na.locf(x)
  } else stop("Invalid data structure.", call. = TRUE)
  invisible(obj)
})

#' @title Get symbol for `TDIResult`
#' @description 
#' Return the symbol for input object of class `TDIResult`.
#' @docType methods
#' @family TDIResult generics
#' @param obj An object of class `TDIResult`.
#' @return Symbol as character.
#' @export
setGeneric("getSymbol", 
  def = function(obj) standardGeneric("getSymbol")
)
#' @rdname getSymbol
setMethod("getSymbol", signature("TDIResult"), function(obj) {
  return(obj@.symbol)
})

#' @title Get series for `TDIResult`
#' @description 
#' Return the series for input object of class `TDIResult`.
#' @docType methods
#' @family TDIResult generics
#' @param obj An object of class `TDIResult`.
#' @return Series as `xts` time-series.
#' @export
setGeneric("getSeries", 
  def = function(obj) standardGeneric("getSeries")
)
#' @rdname getSeries
setMethod("getSeries", signature("TDIResult"), function(obj) {
  return(obj@.series)
})
