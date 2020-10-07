#' @title Yahoo Finance API driver class and methods.
#' @description 
#' Driver class to implement Yahoo Finance API access, simply called `yahoo`.
#' @docType class
#' @rdname YahooDrv-class
#' @keywords internal
#' @include TDIDriver.R
#' @export
setClass("yahoo", contains = "TDIDriver")

#' @rdname YahooDrv-class
#' @include yahooAPI.R
#' @export
setMethod("apiConnect", "yahoo", function(obj, ...) {
  message("Connecting with Yahoo Finance API.")
  con <- new("YahooAPI",
    .drv = obj,
    .conn_args = list(
      baseURL = "https://query1.finance.yahoo.com",
      chart_range = "5y",
      chart_interval = "1d"
    ),
    .endpoints = list(
      series = "/v8/finance/chart"
    )
  )
  invisible(con)
})