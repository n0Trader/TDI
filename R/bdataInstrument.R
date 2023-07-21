#' @title Börsdata API instrument data mapper
#' @description
#' Börsdata API implementation to create an instrument.
#' This class is a simple data mapper.
#' @import R6
#' @export
bdataInstrument <- R6::R6Class("bdataInstrument", inherit = TDIInstrument,
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  # Implementation with object initialization.
  public = list(
    #' @description
    #' Instrument object initialization for Börsdata API.
    #' @param source API source for the data.
    #' @param symbol Symbol as instrument identification.
    #' @param name Name for the instrument.
    #' @param exchange Market for the instrument.
    #' @param sector Main sector of the business.
    #' @param industry Main industry of the business.
    #' @param country Country of registration.
    #' @return An object of class `Instrument`.
    initialize = function(source, symbol, name, exchange, sector, industry, country) {
      stopifnot(
        is.String(source),
        is.String(symbol),
        is.String(name),
        is.String(exchange),
        is.String(sector),
        is.String(industry),
        is.String(country)
      )

      # Set instrument fields.
      self$source <- source
      self$symbol <- symbol
      self$type <- 'Equity'
      self$name <- name
      self$exchange <- exchange
      self$sector <- sector
      self$industry <- industry
      self$country <- country
      invisible(self)
    }
  )
)
