#' @title IEX instrument data mapper
#' @description 
#' IEX Cloud API implementation to create an instrument.
#' This class is a simple data mapper.
#' @import R6
#' @export
iexInstrument <- R6::R6Class("iexInstrument", inherit = TDIInstrument,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  
  # Implementation with object initialization.
  public = list(
    #' @description 
    #' Instrument object initialization for IEX Cloud API. 
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
      invisible(self)
    }
  )
)