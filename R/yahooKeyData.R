#' @title Yahoo Key Data mapper (R6 class constructor)
#' @description 
#' Yahoo Finance API implementation to retrieve instrument key data.
#' This class is a simple data mapper.
#' @import R6
#' @export
yahooKeyData <- R6::R6Class("yahooKeyData", inherit = TDIKeyData,
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  # Implementation with public initialization.
  public = list(
    #' @description 
    #' Initialization with Yahoo Finance instrument key data mapping. 
    #' @param data Data set returned by the API.
    #' @return An object of class `YahooKeyData`.
    initialize = function(data) {
      # Set TDIKeyData fields.
      self$sharesOutstanding <- data$defaultKeyStatistics$sharesOutstanding$raw;
      self$currency <- data$financialData$financialCurrency
      self$price <- data$financialData$currentPrice$raw;
      self$marketCap <- self$sharesOutstanding * self$price;
      self$ebitda <- data$financialData$ebitda$raw;
      self$trailingEPS <- data$defaultKeyStatistics$trailingEps$raw;
      self$forwardEPS <- data$defaultKeyStatistics$forwardEps$raw;
      self$trailingPE <- self$price / self$trailingEPS;
      self$forwardPE <- self$price / self$forwardEPS;
      invisible(self)
    }
  )
)