#' @title Check if the object is of class Instrument
#' @description 
#' Helper to check the object class.
#' If the input `x` is empty it returns the class name.
#' @param x Object to check.
#' @return Boolean result or alternative the class name.
#' @export
is.Instrument <- function(x = NULL) {
  class = "Instrument"
  if (is.null(x)) { return(class) 
  } else return(inherits(x, class))
}

#' @title Instrument (R6 class constructor)
#' @description 
#' Instrument is a R6 class for all types of (tradeable) market instrument(s).
#' @import R6
#' @export
Instrument <- R6::R6Class(is.Instrument(), inherit = TDIResult,
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  # Extend the TDI result fields.
  public = list(
    #' @field type Indicator type classification.
    type = as.character(),
    #' @field currency Nomination currency.
    currency = as.character(),
    
    #' @description 
    #' Constructor for object(s) of class `Indicator`.
    #' @param source API source for the data.
    #' @param symbol Unique identification symbol.
    #' @param type Indicator type classification.
    #' @param currency Nomination currency.
    #' @return An object of class `Indicator`.
    initialize = function(source, symbol, type = NULL, currency = NULL) {
      stopifnot(is.String(source))
      stopifnot(is.String(symbol))
      stopifnot(any(is.null(type), is.String(type)))
      stopifnot(any(is.null(currency), is.String(currency)))
      
      # Initialize the object.
      self$sources <- list(source)
      self$symbol <- symbol
      self$type <- type
      self$currency <- currency
      invisible(self)
    },
    
    #' @description 
    #' Set series and calculate the return from the close price.
    #' @param x Xts time-series with (historical) data.
    #' @return An object of class `TDIResult`.
    setSeries = function(x) {
      super$setSeries(x)
      
      # Calculate the return (only once).
      if (has.Cl(self$series) && !("Return" %in% colnames(self$series))) {
        close <- Cl(self$series)
        self$series$Return <- zoo::na.fill((close - xts::lag.xts(close))/xts::lag.xts(close), 0)
      }  
      invisible(self)
    },
    
    #' @description 
    #' Retrieve the trading session data for the input date plus `n` periods.
    #' @param date Input date-time.
    #' @param n Optional lag number of sessions.
    #' @return Requested session data.
    getSession = function(date, n = 0) {
      i <- self$series[date, which.i = TRUE] + n
      if ((i > nrow(self$series)) || (identical(i, numeric(0)))) return(NULL)
      else return(self$series[i])
    },
    
    #' @description 
    #' Return the *returns* calculated from the `Close` price(s).
    #' @return Instrument returns time-series.
    getReturn = function() {
      if (is.null(nrow(self$series))) {
        warning("Return not calculated due to missing data.")
        return(NULL)
      }
      return(self$series$Return)
    },
    
    #' @description 
    #' Return the *wealth index* calculated from the returns.
    #' @return Instrument returns time-series.
    getWealthIndex = function() {
      if (is.null(nrow(self$series))) {
        warning("Wealth index not calculated due to missing data.")
        return(NULL)
      }
      return(cumprod(self$series$Return + 1))
    }
  )
)
