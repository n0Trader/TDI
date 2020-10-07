#' @title IEX Cloud API driver class and methods.
#' @description 
#' Driver class to implement IEX Cloud API access, simply called `iex`.
#' @docType class
#' @rdname iexDrv-class
#' @keywords internal
#' @include TDIDriver.R
#' @export
setClass("iex", contains = "TDIDriver")

#' @rdname iexDrv-class
#' @include iexAPI.R
#' @export
setMethod("apiConnect", "iex", function(obj, ...) {
  message("Connecting with IEX Cloud API.")
  con <- new("iexAPI",
    .drv = obj,
    .conn_args = list(
      baseURL = Sys.getenv("iex_base_url"),
      api_token = Sys.getenv("iex_token"),
      api_version = Sys.getenv("iex_version"),
      chart_range = "3m",
      chart_interval = "1d"
    ),
    .endpoints = list(
      series = "/stock/%s/chart"
    )
  )
  invisible(con)
})