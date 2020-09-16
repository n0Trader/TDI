#' @include yahooAPI.R
NULL

#' @title IEX Cloud API driver
#' @description 
#' Helper function to establish a connection with IEX Cloud API.
#' @rdname iexDrv-class
#' @export
IEXDriver <- function() {
  if (nargs() > 0) {
    warning("All arguments to the IEX driver are ignored.", call. = FALSE)
  }
  TDIConnector$connect("iex")
}

#' @title IEX Cloud API driver class and methods.
#' @description 
#' Driver class to implement IEX Cloud API access.
#' 
#' The driver class `iex` is named with commonly used source name.
#' This makes it simpler to keep references to the driver with the data.
#' 
#' @docType class
#' @rdname iexDrv-class
#' @include TDIDriver.R
#' @keywords internal
#' @export
setClass("iex", contains = "TDIDriver")

#' @rdname iexDrv-class
#' @export
setMethod("apiConnect", "iex", function(obj, ...) {
  if (file.exists("./config.R")) {
    source('./config.R', local = TRUE)
  } else stop("No config.R found in: ", getwd(), ".",call. = TRUE)
  
  if (exists("iex_params")) {
    message("Connecting with IEX Cloud API.")
    con <- new("iexAPI",
      .drv = obj,
      .conn_args = list(
        baseURL = iex_params("base_url"),
        api_token = iex_params("api_token"),
        api_version = iex_params("api_version"),
        chart_range = "3m",
        chart_interval = "1d"
      ),
        .endpoints = list(
        quotes = "/stock/%s/chart"
      )
    )
    invisible(con)
  } else stop("Missing IEX configuration parameters.", call. = TRUE)
})