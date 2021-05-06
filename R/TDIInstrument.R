#' @title Check if the object is of class Instrument
#' @description 
#' Helper to check the object class.
#' If the input `x` is empty it returns the class name.
#' @param x Object to check.
#' @return Boolean result or alternative the class name.
#' @export
is.Instrument <- function(x = NULL) {
  class = "TDIInstrument"
  if (is.null(x)) { return(class) 
  } else return(inherits(x, class))
}

#' @title Instrument (R6 class constructor)
#' @description 
#' Instrument is an abstract class for (tradeable) market instrument(s).
#' The class provides generic methods and defines the instrument data structure.
#' @details
#' The instrument class and its generic methods are to be used by calling packages.
#' The class is abstract and should be implemented for each API.
#' @import R6
#' @export
TDIInstrument <- R6::R6Class(is.Instrument(), inherit = TDIResult,
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  # Extend the TDI result fields.
  public = list(
    #' @field type Instrument type classification.
    type = as.character(),
    #' @field name Name for the instrument.
    name = as.character(),
    #' @field exchange Exchange for the instrument.
    exchange = as.character(),
    #' @field sector Business sector.
    sector = as.character(),
    #' @field industry Business industry.
    industry = as.character(),
    #' @field currency Denomination currency.
    currency = as.character(),
    #' @field country Country of registration.
    country = as.character(),
    #' @field city City of registration.
    city = as.character(),
    #' @field description Instrument description.
    description = as.character(),
    #' @field website Website URL.
    website = as.character(),
    #' @field keyData Key data object.
    keyData = NA,

    #' @description 
    #' Set series or add additional column(s) to the existing data.
    #' In case the input is a data-frame it is converted to `xts`.
    #' @param x Xts time-series with (historical) data.
    #' @return An object of class `TDIResult`.
    setSeries = function(x) {
      # Check if there is existing data.
      if (!is.na(self$series)) {
        stopifnot(xts::is.xts(x))
        
        # Mark duplicates to be dropped.
        drop <- which(colnames(self$series) %in% colnames(x))
        if (length(drop) == length(colnames(self$series))) {
          # Drop all means we can replace X with Y.
          self$series <- x
        } else if (length(drop) > 0) {
          # Merge y with x minus dropped column(s).
          self$series <- cbind(self$series[, -drop], x)
        } else {
          # No duplicates to drop.
          self$series <- cbind(self$series, x)
        }
        
      } else if (is.data.frame(x)) {
        self$series <- zoo::na.locf(xts::as.xts(x[,-1], order.by = x$Date))
      } else if (xts::is.xts(x)) {
        self$series <- zoo::na.locf(x)
      } else stop("Invalid data structure.", call. = TRUE)
      
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

