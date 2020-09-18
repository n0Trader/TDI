#' @title Yahoo Finance API
#' @description 
#' Yahoo Finance API class inheriting from [TDIConnection-class].
#' This class implements the Yahoo Finance API requests.
#' 
#' @name YahooAPI-class
#' @keywords internal
#' @include TDIConnection.R
#' @export
setClass("YahooAPI", contains = "TDIConnection")

#' @rdname YahooAPI-class
setMethod("initialize", "YahooAPI", function(.Object, ...) {
  .Object <- callNextMethod() # initiate object from parameters
  invisible(.Object)
})

#' @rdname YahooAPI-class
#' @importFrom httr modify_url
setMethod("request", "YahooAPI", function(obj, path, query) {
  # Contruct final URL and execute the request.
  url <- httr::modify_url(obj@.conn_args$baseURL, path = path)
  return(reqJSON(obj, url, query))
})

#' @rdname YahooAPI-class
setMethod("validRange", "YahooAPI", function(obj, range) {
  if (isTRUE(range %in% c("1d", "5d", "1mo", "3mo", "6mo", "1y", "2y", "5y", "10y", "ytd", "max"))) { 
    return(range)
  } else return(obj@.conn_args$chart_range)
})

#' @rdname YahooAPI-class
setMethod("validInterval", "YahooAPI", function(obj, interval) {
  if (isTRUE(interval %in% c("1d", "1w", "1mo"))) { 
    return(interval)
  } else return(obj@.conn_args$chart_interval)
})

#' @rdname YahooAPI-class
#' @importFrom xts xts
#' @importFrom zoo na.locf
setMethod("getSeries", "YahooAPI", function(obj, symbol, range, from, to, interval) {
  stopifnot(all(is.character(symbol), nchar(symbol) > 0))
  message("Downloading: ", symbol, " (source: ", class(obj@.drv), ").")

  # Set endpoint path and query parameters.
  path <- paste(obj@.endpoints$series, symbol, sep = "/")
  if (!is.null(from)) {
    # Specify period by specific dates. 
    from <- convertDate2Unix(from)
    to <- convertDate2Unix(ifelse(is.null(to), Sys.Date(), to))

    # Query parameters with series period dates.
    params <- list(
      "period1" = from,
      "period2" = to,
      "interval" = validInterval(obj, interval),
      "includeTimestamps" = TRUE
    )
  } else {
    # Query parameters with series range.
    params <- list(
      "range" = validRange(obj, range),
      "interval" = validInterval(obj, interval),
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
    return(zoo::na.locf(xts::as.xts(res[,-1], order.by = res$Date)))
  } else {
    warning(paste(res$chart$error$code, res$chart$error$description, sep = ": "))
    return(null)
  }
})