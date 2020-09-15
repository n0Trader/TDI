#' @title TDIConnector
#' @description 
#' The `TDIConnector` manages `TDIConnection`(s).
#' It's purpose is to establish connection(s) to acccess API(s) for trading data.
#' Each API has its own driver that is identified by the `source` (class).
#' 
#' The `connect()` method returns a connection to be used for API access,
#' which is an object of class `TDIConnection`. 
#' Based upon the `source` (driver class) a driver object of class [TDIDriver-class] is loaded. 
#' The driver initialized a connection object of class [TDIConnection-class].
#' 
#' The class implements the **singleton pattern** to re-use connections. 
#' Note that the class is immediately instantiated.
#' 
#' @docType class
#' @name TDIConnector-class
#' @family TDI classes
#' @include TDIDriver.R
#' @export
#' @examples
#' # TDI library creates an singleton object of class `TDIConnector`.
#' # It's `connect()` method is to be used to establish a connection,
#' # that faciliates the API request.
#' con <- TDI::TDIConnector$connect("yahoo")
#' getSymbols(con, "MSFT")
#' 
TDIConnector <- setRefClass("TDIConnector",
  fields = list(
    #' @field .connections List with connection pool.
    .connections = "list"
  ),
  methods = list(
    connect = function(source) {
      stopifnot(nchar(source) > 0)
      if (is.null(.connections[[source]])) {
        # Create new driver to establish the API connection.
        # The API connection is stored in the connection(s) pool.
        drv <- driver(source)
        .connections[[source]] <<- apiConnect(drv)
      }
      return(.connections[[source]])
    }
  )
)$new()
