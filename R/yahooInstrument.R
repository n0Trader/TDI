#' @title Yahoo Finance instrument data mapper
#' @description 
#' Yahoo Finance API implementation to create an instrument.
#' This class is a simple data mapper.
#' @import R6
#' @export
yahooInstrument <- R6::R6Class("yahooInstrument", inherit = TDIInstrument,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  
  # Implementation with object initialization.
  public = list(
    #' @description 
    #' Instrument object initialization for Yahoo Finance API. 
    #' @param source API source for the data.
    #' @param symbol Symbol as instrument identification.
    #' @param json Json string returned by the API.
    #' @return An object of class `Instrument`.
    initialize = function(source, symbol, json) {
      stopifnot(is.String(source))
      stopifnot(is.String(symbol))
      
      # Set instrument fields.
      self$source <- source
      self$symbol <- symbol
      
      if (!missing(json)) {
        self$sector <- json$summaryProfile$sector
        self$industry <- json$summaryProfile$industry
        self$country <- json$summaryProfile$country
        self$city <- json$summaryProfile$city
        self$description <- json$summaryProfile$longBusinessSummary
        self$website <- json$summaryProfile$website
        self$currency <- json$defaultKeyStatistics$financialCurrency
        self$keyData <- yahooKeyData$new(json)
        lapply(json$cashflowStatementHistoryQuarterly, print)
      }
      invisible(self)
    }
  )
)