#' @title Yahoo driver (R6 class constructor)
#' @description 
#' Driver class to implement Yahoo Finance API access, simply called `yahoo`.
#' @import R6
#' @export
yahoo <- R6::R6Class("yahoo", inherit = TDIDriver,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  lock_class = TRUE, # lock the interface

  # Abstract API driver methods (with error messages).
  public = list(
    #' @description 
    #' Setup API connection.
    #' @return An object of type `TDIConnection`.
    connect = function() {
      message("Connecting with Yahoo Finance API.")
      con <- yahooAPI$new(driver = self,
        # Arguments required to access the API.
        conn_args = list(
          baseURL = "https://query2.finance.yahoo.com",
          chart_range = "5y",
          chart_interval = "1d"
        ),
        # Routes to endpoints.
        endpoints = list(
          details = "/v10/finance/quoteSummary/%s?modules=summaryProfile,defaultKeyStatistics,financialData",
          financial = "/v10/finance/quoteSummary/%s?modules=",
          chart = "/v8/finance/chart/%s"
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
