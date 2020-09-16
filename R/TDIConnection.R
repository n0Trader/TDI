#' @title TDIConnection class
#' @description 
#' Wraps an object of class [TDIDriver-class] and the connection parameters.
#' Its purpose is to establish a connection and to store all connection details.
#' 
#' Connection(s) are established by the `TDIConnector`,
#' which keeps a connection pool to re-use connection(s).
#' 
#' @docType class
#' @include TDIDriver.R
#' @name TDIConnection-class
#' @family TDI classes
#' @family TDIConnection generics
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

#' @title Execute API request
#' Call the API endpoint with the relative URL path and query parameters.
#' @import httr
#' @family TDIConnection generics
#' @param obj An object of [TDIConnection-class].
#' @param path Relative path for the endpoint.
#' @param query URL query arguments.
#' @export
setGeneric("request",
  def = function(obj, path, query) standardGeneric("request")
)

#' @title Validate the API range parameter
#' Generic validation method for API range parameter.
#' @family TDIConnection generics
#' @param obj An object of [TDIConnection-class].
#' @param range Range query parameter.
#' @export
setGeneric("validRange",
  def = function(obj, range = NULL) standardGeneric("validRange")
)

#' @title Validate the API interval parameter
#' Generic validation method for API interval parameter.
#' @family TDIConnection generics
#' @param obj An object of [TDIConnection-class].
#' @param interval Interval query parameter.
#' @export
setGeneric("validInterval",
  def = function(obj, interval = NULL) standardGeneric("validInterval")
)

#' @title Execute API request with JSON
#' Generic helper method to execute JSON requests.
#' @rdname TDIConnection-class
#' @import httr
#' @param obj An object of [TDIConnection-class].
#' @param url Final URL for the endpoint.
#' @param query URL query arguments.
#' @export
setGeneric("reqJSON", def = function(obj, url, query) standardGeneric("reqJSON"))
setMethod("reqJSON", "TDIConnection", function(obj, url, query = NULL) {
  stopifnot(any(is.null(query), is.list(query)))
  
  # Call the URL and check the results.
  resp <- httr::GET(url, query = query)
  if (httr::status_code(resp) != 200) {
    warning(paste0("API request failed with HTTP status: ", httr::status_code(resp)))
  } else if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  # Parse the JSON results and check the results.
  resp <- try(jsonlite::fromJSON(httr::content(resp, "text")), silent = TRUE)
  if (inherits(resp, "try-error")) {
    warning(res[1])
    return(NULL)
  } else return(resp)
})

#' @title Retrieve symbols historical prices
#' Retrieves historical prices for the symbols.
#'
#' @family TDIConnection generics
#' @param obj An object of [TDIConnection-class].
#' @param symbol Symbol to identify the instrument.
#' @param range Optional period range.
#' @param from Optional start date of period.
#' @param to Optional end date of period.
#' @param interval Optional interval to retrieve quotes.
#' @param ... Other arguments.
#' @export
setGeneric("getSymbol",
  def = function(obj, symbol, range = NULL, from = NULL, to = NULL, interval = NULL, ...) standardGeneric("getSymbol")
)
