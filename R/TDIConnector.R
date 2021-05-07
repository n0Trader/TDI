#' @title TDIConnector
#' @description 
#' TDIConnector is an instantiation of class `TDIConnector` to manage connection(s).
#' It's purpose is to establish connection(s) to acccess API(s) for qualitative data.
#' @details
#' The `TDIConnector` connects with an API through its driver identified by `source` (class).
#' It's `connect()` method establishes the API access using the `TDIDriver`,
#' and returns that connection as an object of class `TDIConnection`. 
#' 
#' The class implements the **singleton pattern** to re-use connections. 
#' Note that the class is immediately instantiated.
#' @import R6
#' @export
#' @usage TDIConnector(source = "yahoo")
#' @examples
#' # TDI library creates an singleton object of class `TDIConnector`.
#' # It's `connect()` method is to be used to establish a connection,
#' # that facilitates the API request.
#' con <- TDI::TDIConnector$connect(source = "yahoo")
#' msft <- con$getChart(symbol = "MSFT")
TDIConnector <- R6::R6Class("TDIConnector",
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  public = list(
    initialize = function() { invisible(self) },
    
    #' @description
    #' Setup and provide an API connection.
    #' @param source API driver class name.
    #' @return An object of class `TDIConnection`.
    connect = function(source) {
      stopifnot(is.String(source))

      if (is.null(private$.Connections[[source]])) {
        # Create new driver to establish the API connection.
        # The API connection is stored in the connection(s) pool.
        drv <- driver(source)
        stopifnot(is.TDIDriver(drv))
        private$.Connections[[source]] <- drv$connect()
      }
      return(private$.Connections[[source]])
    }
  ),
  
  private = list(
    # Connection pool.
    .Connections = list()
  )
)$new()