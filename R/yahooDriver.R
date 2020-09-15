#' @include yahooAPI.R
NULL

#' @title Yahoo Finance API driver
#' @description 
#' Helper function to establish a connection with Yahoo Finance API.
#' @rdname YahooDrv-class
#' @export
YahooDriver <- function() {
  if (nargs() > 0) {
    warning("All arguments to the Yahoo driver are ignored.", call. = FALSE)
  }
  TDIConnector$connect("yahoo")
}

#' @title Yahoo Finance API driver class and methods.
#' @description 
#' Driver class to implement Yahoo Finance API access.
#' 
#' The driver class `yahoo` is named with commonly used source name.
#' This makes is simpler to keep references to the driver with the data.
#' 
#' @docType class
#' @rdname YahooDrv-class
#' @include TDIDriver.R
#' @keywords internal
#' @export
setClass("yahoo", contains = "TDIDriver")

#' @rdname YahooDrv-class
#' @export
setMethod("apiConnect", "yahoo", function(obj, ...) {
  message("Connecting with Yahoo Finance API.")
  con <- new("YahooAPI",
    .drv = obj,
    .conn_args = list(
      baseURL = "https://query1.finance.yahoo.com"
    ),
    .endpoints = list(
      quotes = "/v8/finance/chart/"
    )
  )
  invisible(con)
})