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
    #' @param json Json string returned by the API.
    #' @return An object of class `YahooKeyData`.
    initialize = function(json) {
      # Set TDIKeyData fields.
      self$sharesOutstanding <- json$defaultKeyStatistics$sharesOutstanding$raw;
      self$price <- json$financialData$currentPrice$raw;
      self$marketCap <- self$sharesOutstanding * self$price;
      self$ebitda <- json$financialData$ebitda$raw;
      self$trailingEPS <- json$defaultKeyStatistics$trailingEps$raw;
      self$forwardEPS <- json$defaultKeyStatistics$forwardEps$raw;
      self$trailingPE <- self$price / self$trailingEPS;
      self$forwardPE <- self$price / self$forwardEPS;
      invisible(self)
    }
  )
)