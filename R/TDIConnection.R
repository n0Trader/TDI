#' @title TDIConnection class
#' @description 
#' Wraps objects of the [TDIDriver-class] class with connection options.
#' The purpose is to store both the driver and the connection options.
#' 
#' API access can be established through [apiConnect()], 
#' passing only the object with optional additional arguments.
#'
#' @docType class
#' @name TDIConnection-class
#' @family TDI classes
#' @family TDIConnection generics
#' @export
#' @include TDIDriver.R
#' @examples
#' # TO-DO
setClass("TDIConnection",
  slots = c(".drv" = "TDIDriver", ".conn_args" = "list", ".endpoints" = "list"),
  contains = c("TDIObject")
)

#' @title Class constructor TDIConnection
#' @description 
#' Initialize object(s) of class TDIConnection.
#' 
#' @name TDIConnection-class
#' @family TDI classes
setMethod("initialize", "TDIConnection", function(.Object, ...) {
  .Object <- callNextMethod() # initiate object from parameters
  year <- as.numeric(format(Sys.Date(), "%Y")) - 5
  .Object@.conn_args$from <- as.Date(paste(year, "01-01", sep = "-"))
  .Object@.conn_args$range <- "2y"
  .Object@.conn_args$interval <- "1d"
  invisible(.Object)
})

#' @title Retrieve symbols historical prices
#' Retrieves historical prices for the symbols.
#'
#' @param obj An object of [TDIConnection-class].
#' @param symbol Symbol to identify the instrument.
#' @param ... Other arguments.
#' @family TDIDriver generics
#' @inherit TDItest::spec_get_info return
#' @export
setGeneric("getSymbol",
  def = function(obj, symbol, ...) standardGeneric("getSymbol")
)
