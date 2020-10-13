#' @title FRED API driver (R6 class constructor).
#' @description 
#' Driver class to implement FRED API access, simply called `fred`.
#' @import R6
#' @export
fred <- R6::R6Class("fred", inherit = TDIDriver,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  lock_class = TRUE, # lock the interface
  portable = TRUE, # enable inheritance across packages
  
  # Abstract API driver methods (with error messages).
  public = list(
    #' @description 
    #' Setup API connection.
    #' @return An object of type `TDIConnection`.
    connect = function() {
      message("Connecting with Federal Reserve St. Louis API.")
      warning("This product uses the FRED(R) API but is not endorsed or certified by the Federal Reserve Bank of St. Louis.")
      con <- fredAPI$new(driver = self,
        # Arguments required to access the API.
        conn_args = list(
          baseURL = Sys.getenv("fred_base_url"),
          api_key = Sys.getenv("fred_api_key")
        ),
        # Routes to endpoints.
        endpoints = list(
          chart = "/fred/series/observations"
        ),
        # Valid parameter values (default = [1]).
        values = list (
          
        )
      )
      
      invisible(con)
    }
  )
)