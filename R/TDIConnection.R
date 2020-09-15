#' @title TDIConnection class
#' @description 
#' Wraps an object of class [TDIDriver-class] and the connection parameters.
#' Its purpose is to establish a connection and to store all connection details.
#' 
#' Connection(s) are established by the `TDIConnector`,
#' which keeps a connection pool to re-use connection(s).
#' 
#' @docType class
#' @name TDIConnection-class
#' @family TDI classes
#' @family TDIConnection generics
#' @include TDIObject.R
#' @export
setClass("TDIConnection", contains = c("TDIObject"),
  slots = list(
    #' @slot .drv Object of class [TDIDriver-class].
    ".drv" = "TDIDriver", 
    #' @slot .conn_args List with connection parameters.
    ".conn_args" = "list", 
    #' @slot .endpoints List with available API endpoints.
    ".endpoints" = "list"
  )
)

#' @title Class constructor TDIConnection
#' @description 
#' Initialize object(s) of class `TDIConnection`.
#' 
#' @name TDIConnection-class
#' @family TDI classes
setMethod("initialize", "TDIConnection", function(.Object, ...) {
  .Object <- callNextMethod() # initiate object from parameters
  invisible(.Object)
})

#' @title Retrieve symbols historical prices
#' Retrieves historical prices for the symbols.
#'
#' @family TDIConnection generics
#' @param obj An object of [TDIConnection-class].
#' @param symbol Symbol to identify the instrument.
#' @param ... Other arguments.
#' @export
setGeneric("getSymbol",
  def = function(obj, symbol, ...) standardGeneric("getSymbol")
)
