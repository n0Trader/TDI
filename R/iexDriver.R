#' @title IEX Cloud API driver (R6 class constructor).
#' @description 
#' Driver class to implement IEX Cloud API access, simply called `iex`.
#' @import R6
#' @export
iex <- R6::R6Class("iex", inherit = TDIDriver,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  lock_class = TRUE, # lock the interface

  # Abstract API driver methods (with error messages).
  public = list(
    #' @description 
    #' Setup API connection.
    #' @return An object of type `TDIConnection`.
    connect = function() {
      message("Connecting with IEX Cloud API.")
      con <- iexAPI$new(driver = self,
        # Arguments required to access the API.
        conn_args = list(
          baseURL = Sys.getenv("iex_base_url"),
          api_token = Sys.getenv("iex_token"),
          api_version = Sys.getenv("iex_version"),
          chart_range = "3m",
          chart_interval = "1d"
        ),
        # Routes to endpoints.
        endpoints = list(
          chart = "%s/stock/%s/chart/%s"
        ),
        # Valid parameter values (default = [1]).
        values = list(
          range = c("3m", "5y", "2y", "1y", "ytd", "6m", "1m", "5d"),
          interval = c("1d", "1w", "1mo")
        )
      )
      
      invisible(con)
    }   
  )
)
