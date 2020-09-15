# Setup Mock test classes.
MockDriver <- setClass("mock", contains = "TDIDriver")
MockAPI <- setClass("MockAPI", contains = "TDIConnection")

setMethod("apiConnect", "mock", function(obj, ...) {
  con <- new("MockAPI",
    .drv = obj,
    .conn_args = list(
      baseURL = "https://mock.com"
    ),
    .endpoints = list(
      quotes = "/quotes/"
    )
  )
  invisible(con)
})