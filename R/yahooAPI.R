#' @title Yahoo Finance API
#' @description 
#' Yahoo Finance API class inheriting from `TDIConnection`.
#' This class implements the Yahoo Finance API requests.
#' @import R6
#' @export
YahooAPI <- R6::R6Class("YahooAPI", inherit = TDIConnection,
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  # Implement API driver endpoints.
  public = list(
    #' @description 
    #' Retrieve historical prices for the symbol.
    #' @param ... see \code{\link{TDIConnection}}
    #' @return An object of class `Instrument` with historical prices.
    getChart = function(...) {
      args <- super$getChart(...)

      # Check if the API query is by period or by range.      
      if (!is.null(args$from)) {
        # Query parameters with series period by dates.
        params <- list(
          "period1" = convertDate2Unix(args$from),
          "period2" = convertDate2Unix(defaultToday(args$to)),
          "interval" = self$validValue(interval = args$interval),
          "includeTimestamps" = TRUE
        )
      } else {
        # Query parameters with series range.
        params <- list(
          "range" = self$validValue(range = args$range),
          "interval" = self$validValue(interval = args$interval),
          "includeTimestamps" = TRUE
        )
      }
      
      # Execute the Json API request with URL request string.
      res <- self$jsonRequest(
        self$requestString(endpoint = "chart", path = list(args$symbol), params)
      )
      
      if (!utils::hasName(res, "chart")) {
        warning("Unexpected response from Yahoo Finance API.")
        return(NULL)
      }
      
      # Check the response and handle the results.
      if (is.null(res$chart$error)) {
        # Convert the results into a new instrument object.
        # Note; other data to be considered for later.
        instr <- Instrument$new(
          source = as.character(class(self$driver)[1]),
          symbol = as.character(args$symbol),
          currency = res$chart$result$meta$currency,
          type = res$chart$result$meta$instrumentType
        )
        
        # Add historical prices to the instrument.
        instr <- instr$setSeries(data.frame(
          Date = convertUnix2Date(res$chart$result$timestamp[[1]]),
          Open = res$chart$result$indicators$quote[[1]]$open[[1]],
          High = res$chart$result$indicators$quote[[1]]$high[[1]],
          Low = res$chart$result$indicators$quote[[1]]$low[[1]],
          Close = res$chart$result$indicators$quote[[1]]$close[[1]],
          Volume = res$chart$result$indicators$quote[[1]]$volume[[1]]
        ))
        return(instr)
        
      } else {
        warning(paste(res$chart$error$description, symbol, sep = ": "))
        return(NULL)
      }
      
    }
  )
)