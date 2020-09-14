#' @title Yahoo Finance API.
#' @description 
#' Yahoo Finance API class inheriting from [TDIConnection-class].
#' Objects of this class provide the Yahoo API methods.
#' 
#' @docType class
#' @rdname YahooAPI-class
#' @keywords internal
#' @export
setClass("YahooAPI", contains = "TDIConnection")

#' @rdname YahooAPI-class
#' @importFrom curl curl
#' @importFrom jsonlite fromJSON
#' @importFrom xts xts
#' @importFrom zoo as.Date
#' @param range A period range.
#' @param from Start date of period.
#' @param to End date of period.
#' @param interval Interval to retrieve quotes.
#' @export
setMethod("getSymbol", "YahooAPI", function(obj, symbol, range, from, to, interval = obj@.conn_args$interval) {
  browser()
  stopifnot(nchar(symbol) > 0)
  message("Downloading: ", symbol, " (source: ", class(obj), ").")
  
  # Interval with a period range or period specified by two dates.
  if (missing(range)) {
    from <- convertDate2Unix(ifelse(missing(from), obj@.conn_args$from, from))
    to <- convertDate2Unix(ifelse(missing(to), Sys.Date(), to))
    interval  <- ifelse(interval %in% c("1d", "1w", "1m"), interval, obj@.conn_args$interval)
    
    # Construct endpoint URL with parameters.
    url <- sprintf(paste(obj@.endpoints$quotes, "?period1=", "&period2=", "&interval=", "&includeTimestamps=true", sep = "%s"), symbol, from, to, interval)
  } else {
    range  <- ifelse(range %in% c("1d", "5d", "1mo", "3mo", "6mo", "1y", "2y", "5y", "10y", "ytd", "max"), range, obj@.conn_args$range)
    interval  <- ifelse(interval %in% c("1d", "1w", "1mo"), interval, obj@.conn_args$interval)
    
    # Construct endpoint URL with parameters.
    url <- sprintf(paste(obj@.endpoints$quotes, "?range=", "&interval=", "&includeTimestamps=true", sep = "%s"), symbol, range, interval)
  }

  # Contruct final URL and create connection.
  url <- paste0(obj@.conn_args$baseURL, url)
  h <- curl::new_handle()
  con <- curl::curl(url, handle = h)
  
  # Try downloading results in JSON.
  res <- try(jsonlite::fromJSON(con), silent = TRUE)
  message(url)
  if (inherits(res, "try-error")) {
    message(res[1])
  } else {
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
    return(xts::as.xts(res[,-1], order.by = res$Date))
  }
})


