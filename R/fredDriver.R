#' @title FRED economic data API driver
#' @description 
#' Helper function to establish a connection with FRED API.
#' @rdname iexDrv-class
#' @export
IEXDriver <- function() {
  if (nargs() > 0) {
    warning("All arguments to the FRED driver are ignored.", call. = FALSE)
  }
  TDIConnector$connect("fred")
}

#' @title FRED API driver class and methods.
#' @description 
#' Driver class to implement FRED API access, simply called `fred`.
#' @docType class
#' @rdname fredDrv-class
#' @keywords internal
#' @include TDIDriver.R
#' @export
setClass("fred", contains = "TDIDriver")

#' @rdname fredDrv-class
#' @include fredAPI.R
#' @export
setMethod("apiConnect", "fred", function(obj, ...) {
  if (file.exists("./config.R")) {
    source('./config.R', local = TRUE)
  } else stop("No config.R found in: ", getwd(), ".",call. = TRUE)
  
  if (exists("fred_params")) {
    message("Connecting with Federal Reserve St. Louis API.")
    warning("This product uses the FRED(R) API but is not endorsed or certified by the Federal Reserve Bank of St. Louis.")
    con <- new("fredAPI",
      .drv = obj,
      .conn_args = list(
        baseURL = fred_params("base_url"),
        api_key = fred_params("api_key")
      ),
      .endpoints = list(
        series = "/series/observations"
      )
    )
    invisible(con)
  } else stop("Missing FRED configuration parameters.", call. = TRUE)
})