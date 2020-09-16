#' @title Yahoo Finance API.
#' @description 
#' Yahoo Finance API class inheriting from [TDIConnection-class].
#' This class implements the Yahoo Finance API requests.
#' 
#' @docType class
#' @name YahooAPI-class
#' @keywords internal
#' @slot .handle Curl connection handle.
#' @include TDIConnection.R
#' @export
setClass("YahooAPI", contains = "TDIConnection",
  # Workaround for unknown class in slots.
  slots = c(".handle")
)

#' @title Class constructor YahooAPI
#' @description 
#' Initialize object(s) of class `YahooAPI`.
#' 
#' @name YahooAPI-class
setMethod("initialize", "YahooAPI", function(.Object, ...) {
  .Object <- callNextMethod() # initiate object from parameters
  
  # Set connection defaults.
  year <- as.numeric(format(Sys.Date(), "%Y")) - 5
  .Object@.conn_args$from <- as.Date(paste(year, "01-01", sep = "-"))
  .Object@.conn_args$range <- "2y"
  .Object@.conn_args$interval <- "1d"
  
  # Curl constructs a handle for the connection.
  .Object@.handle <- curl::new_handle()
  
  invisible(.Object)
})

#' @rdname YahooAPI-class
#' @importFrom httr modify_url
setMethod("request", "YahooAPI", function(obj, path, query = NULL) {
  # Contruct final URL and execute the request.
  url <- httr::modify_url(obj@.conn_args$baseURL, path = path)
  return(reqJSON(obj, url, query))
})

#' @rdname YahooAPI-class
#' @importFrom xts xts
#' @export
setMethod("getSymbol", "YahooAPI", function(obj, symbol, range, from, to, interval = obj@.conn_args$interval) {
  stopifnot(nchar(symbol) > 0)
  message("Downloading: ", symbol, " (source: ", class(obj@.drv), ").")
  
  # Set endpoint with query parameters.
  path <- paste(obj@.endpoints$quotes, symbol, sep = "/")
  interval  <- ifelse(interval %in% c("1d", "1w", "1mo"), interval, obj@.conn_args$interval)
  if (!missing(from)) {
    # Specify period by specific dates. 
    from <- convertDate2Unix(from)
    to <- convertDate2Unix(ifelse(missing(to), Sys.Date(), to))

    # Query parameters.
    params <- list(
      "period1" = from,
      "period2" = to,
      "interval" = interval,
      "includeTimestamps" = TRUE
    )
  } else {
    # Specify period by a date range.
    range  <- ifelse(range %in% c("1d", "5d", "1mo", "3mo", "6mo", "1y", "2y", "5y", "10y", "ytd", "max"), range, obj@.conn_args$range)

    # Query parameters.
    params <- list(
      "range" = range,
      "interval" = interval,
      "includeTimestamps" = TRUE
    )
  }

  # Execute the API request.
  res <- request(obj, path, params)
  if (is.null(res$chart$error)) {
    # Convert the historical prices into a time-series.
    # Note; other data to be considered for later.
    res <- data.frame(
      Date = convertUnix2Date(res$chart$result$timestamp[[1]]),
      Open = res$chart$result$indicators$quote[[1]]$open[[1]],
      High = res$chart$result$indicators$quote[[1]]$high[[1]],
      Low = res$chart$result$indicators$quote[[1]]$low[[1]],
      Close = res$chart$result$indicators$quote[[1]]$close[[1]],
      Volume = res$chart$result$indicators$quote[[1]]$volume[[1]]
    )
    return(na.locf(xts::as.xts(res[,-1], order.by = res$Date)))
  } else {
    warning(paste(res$chart$error$code, res$chart$error$description, sep = ": "))
    return(null)
  }
})


