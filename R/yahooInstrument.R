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
    #' @param data Data set returned by the API.
    #' @return An object of class `Instrument`.
    initialize = function(source, symbol, data) {
      stopifnot(
        is.String(source),
        is.String(symbol)
      )

      # Set instrument fields.
      self$source <- source
      self$symbol <- symbol

      if (!missing(data)) {
        self$sector <- data$summaryProfile$sector
        self$industry <- data$summaryProfile$industry
        self$country <- data$summaryProfile$country
        self$exchange <- data$price$exchangeName
        self$name <- data$price$longName
        self$type <- data$price$quoteType
        if (utils::hasName(data, "defaultKeyStatistics")) {
          self$keyData <- yahooKeyData$new(data)
        }
        if (utils::hasName(data, "cashflowStatementHistory")) {
          self$annualCashFlow <- apply(data$cashflowStatementHistory$cashflowStatements[[1]], 1, function(x) {
            return(yahooCashFlow$new(x))
          })
        }
        if (utils::hasName(data, "cashflowStatementHistoryQuarterly")) {
          self$quarterlyCashFlow <- apply(data$cashflowStatementHistoryQuarterly$cashflowStatements[[1]], 1, function(x) {
            return(yahooCashFlow$new(x))
          })
        }
      }
      invisible(self)
    }
  )
)
