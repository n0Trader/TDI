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
#' @import methods
#' @export
setMethod("apiConnect", "fred", function(obj, ...) {
  message("Connecting with Federal Reserve St. Louis API.")
  warning("This product uses the FRED(R) API but is not endorsed or certified by the Federal Reserve Bank of St. Louis.")
  con <- methods::new("fredAPI",
    .drv = obj,
    .conn_args = list(
      baseURL = Sys.getenv("fred_base_url"),
      api_key = Sys.getenv("fred_api_key")
    ),
    .endpoints = list(
      series = "/series/observations"
    )
  )
  invisible(con)
})