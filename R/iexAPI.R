#' @title IEX Cloud API (R6 constructor class)
#' @description 
#' IEX Cloud API class inheriting from `TDIConnection`.
#' This class implements the IEX Cloud API requests.
#' @seealso API documentation on \strong{\href{https://iexcloud.io/docs/api}{IEX Cloud API website}}.
#' @import R6
#' @export
iexAPI <- R6::R6Class("iexAPI", inherit = TDIConnection,
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
      query <- list(chartByDay = TRUE, token = as.character(self$conn_args$api_token))
      path <- list(version = self$conn_args$api_version, 
        symbol = args$symbol, 
        range = self$validValue(range = args$range)
      )
      res <- self$jsonRequest(
        self$requestString(endpoint = "chart", path = path, query = query)
      )
      
      # Check the response and handle the results.
      if (is.data.frame(res)) {
        # Convert the results into a new instrument object.
        # Note; other data to be considered for later.
        instr <- Instrument$new(
          symbol = as.character(args$symbol),
          source = as.character(class(self$driver)[1])
        )
        
        df <- res[, c("date", "open", "high", "low", "close", "volume")]
        colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume")
        df$Date <- convertUnix2Date(df$Date)
        return(instr$setSeries(df))
      } else return(NULL)
    }
  )
)

#' Set the range from the last date.
#' The purpose is to only retrieve missing dates.
#' @noRd
# .range_set <- function(d = as.Date(0)) {
#   diff <- difftime(Sys.Date(), d, units = "days")
#   if (diff > (2 * 365)) { r = "5y" 
#   } else if (diff > 365) { r = "2y"
#   } else if (diff > 175) { r = "1y"
#   } else if (diff > 75) { r = "6m"
#   } else if (diff > 28) { r = "3m"
#   } else if (diff > 5) { r = "1m"
#   } else if (diff > 1) { r = "5d"
#   } else r = "5d"
#   return(r)
# }
