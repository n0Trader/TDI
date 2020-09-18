#' @title IEX Cloud API
#' @description 
#' IEX Cloud API class inheriting from [TDIConnection-class].
#' This class implements the IEX Cloud API requests.
#' 
#' @docType class
#' @name iexAPI-class
#' @keywords internal
#' @seealso API documentation on \strong{\href{https://iexcloud.io/docs/api}{IEX Cloud API website}}.
#' @include TDIConnection.R
#' @export
setClass("iexAPI", contains = "TDIConnection")

#' @rdname iexAPI-class
setMethod("initialize", "iexAPI", function(.Object, ...) {
  .Object <- callNextMethod() # initiate object from parameters
  invisible(.Object)
})

#' @rdname iexAPI-class
#' @importFrom httr modify_url
setMethod("request", "iexAPI", function(obj, path, query) {
  stopifnot(is.list(query))
  
  # Contruct final URL, merge query params and execute the request.
  endpoint <- c(as.character(obj@.conn_args$api_version), path)
  url <- httr::modify_url(obj@.conn_args$baseURL, path = endpoint)
  query <- c(list(token = as.character(obj@.conn_args$api_token)), query)
  return(reqJSON(obj, url, query))
})

#' @rdname iexAPI-class
setMethod("validRange", "iexAPI", function(obj, range) {
  if (is.null(range)) {
    return(NULL)
  } else if (isTRUE(range %in% c("5y", "2y", "1y", "ytd", "6m", "3m", "1m", "5d"))) { 
    return(range)
  } else return(obj@.conn_args$chart_range)
})

#' @rdname iexAPI-class
setMethod("validInterval", "iexAPI", function(obj, interval) {
  if (isTRUE(interval %in% c("1d", "1w", "1mo"))) { 
    return(interval)
  } else return(obj@.conn_args$chart_interval)
})

#' @rdname iexAPI-class
#' @importFrom xts xts
#' @importFrom zoo na.locf
setMethod("getSymbol", "iexAPI", function(obj, symbol, range, from, to, interval) {
  stopifnot(all(is.character(symbol), nchar(symbol) > 0))
  message("Downloading: ", symbol, " (source: ", class(obj@.drv), ").")

  # Set endpoint with query parameters.
  endpoint <- paste(sprintf(obj@.endpoints$quotes, symbol), validRange(obj,range), sep = "/")
  query <- list(chartByDay = TRUE)
  
  # Execute the API request.
  res <- request(obj, endpoint, query)
  if (is.data.frame(res)) {
    df <- res[, c("date", "open", "high", "low", "close", "volume")]
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume")
    return(zoo::na.locf(xts::as.xts(df[,-1], order.by = convertUnix2Date(df$Date))))
  } else return(null)
})

#' Set the range from the last date.
#' The purpose is to only retrieve missing dates.
#' @noRd
# .range_set <- function(d = as.Date(0)) {
#   diff <- difftime(Sys.Date(), d, units = "days")
#   if (diff > (2 * 365)) { r = "5y" 
#   } else if (diff > 365) { r = "2y"
#   } else if (diff > 175) { r = "1y"
#   } else if (diff > 75) { r = "6m"
#   } else if (diff > 28) { r = "3m"
#   } else if (diff > 5) { r = "1m"
#   } else if (diff > 1) { r = "5d"
#   } else r = "5d"
#   return(r)
# }
