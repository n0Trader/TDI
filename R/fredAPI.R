#' @title FRED economic date API
#' @description 
#' FRED economic data API class inheriting from [TDIConnection-class].
#' This class implements the FRED API requests.
#' 
#' @docType class
#' @name fredAPI-class
#' @keywords internal
#' @seealso API documentation on \strong{\href{https://research.stlouisfed.org/docs/api/}{St. Louis Fed web services website}}.
#' @include TDIConnection.R
#' @export
setClass("fredAPI", contains = "TDIConnection")

#' @rdname fredAPI-class
setMethod("initialize", "fredAPI", function(.Object, ...) {
  .Object <- callNextMethod() # initiate object from parameters
  invisible(.Object)
})

#' @rdname fredAPI-class
#' @import httr
setMethod("request", "fredAPI", function(obj, path, query) {
  stopifnot(is.list(query))
  
  # Contruct final URL, merge query params and execute the request.
  url <- paste0(obj@.conn_args$baseURL, path = path)
  query <- c(list(file_type = "json",
    api_key = as.character(obj@.conn_args$api_key)
    ), query
  )
  return(request.JSON(obj, url, query))
})

#' @rdname fredAPI-class
#' @import xts
#' @import zoo
setMethod("getChart", signature("fredAPI"), function(obj, symbol, range, from, to, interval) {
  stopifnot(all(is.character(symbol), nchar(symbol) > 0))
  message("Downloading: ", symbol, " (source: ", class(obj@.drv), ").")

  # Set endpoint path and execute the request.
  endpoint <- as.character(obj@.endpoints$series)
  query <- list(series_id = symbol)
  res <- request(obj, endpoint, query)
  return(res)
})
