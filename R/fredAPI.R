#' @title FRED economic date API (R6 constructor class)
#' @description 
#' FRED economic data API class inheriting from `TDIConnection`.
#' This class implements the FRED API requests.
#' @seealso API documentation on \strong{\href{https://research.stlouisfed.org/docs/api/}{St. Louis Fed web services website}}.
#' @import R6
#' @export
fredAPI <- R6::R6Class("fredAPI", inherit = TDIConnection,
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  # Implement API driver endpoints.
  public = list(
    #' @description 
    #' Retrieve historical prices for the symbol.
    #' @param ... see \code{\link{TDIConnection}}
    #' @return An object of class `Instrument` with historical prices.
    getChart = function(...) {
      args <- super$getChart(...)
      
      # Execute the Json API request with URL request string.
      query <- list(file_type = "json", 
        series_id = args$symbol, 
        api_key = as.character(self$conn_args$api_key)
      )
      res <- self$jsonRequest(
        self$requestString(endpoint = "chart", query)
      )
      
      return(res)
    }
  )
)
