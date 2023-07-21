#' @title Borsdata driver (R6 class constructor)
#' @description
#' Driver class to implement Borsdata API access, simply called `bdata`.
#' @import R6
#' @export
bdata <- R6::R6Class("bdata", inherit = TDIDriver,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  lock_class = TRUE, # lock the interface

  # Abstract API driver methods (with error messages).
  public = list(
    #' @description
    #' Setup API connection.
    #' @return An object of type `TDIConnection`.
    connect = function() {
      message("Connecting with Borsdata API.")
      con <- bdataAPI$new(driver = self,
        # Arguments required to access the API.
        conn_args = list(
          baseURL = Sys.getenv("bdata_base_url"),
          api_token = Sys.getenv("bdata_token")
        ),
        # Routes to endpoints.
        endpoints = list(
          branches = "/v1/branches",
          countries = "/v1/countries",
          sectors = "/v1/sectors",
          markets = "/v1/markets",
          instruments = "/v1/instruments",
          stockPricesList = "/v1/instruments/stockprices/last",
          chart = "/v1/instruments/%s/stockprices"
        ),
        # Valid parameter values (default = [1]).
        values = list(
          range = c("1d", "5d", "1mo", "3mo", "6mo", "1y", "2y", "5y", "10y", "ytd", "max"),
          interval = c("1d", "1w", "1mo")
        )
      )

      invisible(con)
    }
  )
)
