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
    #' @field source Identifier for the data source.
    source = NULL,
    #' @field symbol Unique identification symbol.
    symbol = NULL,
    #' @field type Instrument type classification.
    type = NULL,
    #' @field name Name for the instrument.
    name = NULL,
    #' @field exchange Exchange for the instrument.
    exchange = NULL,
    #' @field sector Business sector.
    sector = NULL,
    #' @field industry Business industry.
    industry = NULL,
    #' @field country Country of registration.
    country = NULL,
    #' @field keyData Key data object.
    keyData = NA,
    #' @field annualCashFlow Annual cash flow data.
    annualCashFlow = NA,
    #' @field quarterlyCashFlow Quarterly cash flow data.
    quarterlyCashFlow = NA,
    #' @field series Xts time series with (historical) data.
    series = NA,

    #' @description
    #' Set series or add additional column(s) to the existing data.
    #' In case the input is a data-frame it is converted to `xts`.
    #' @param x Xts time series with (historical) data.
    #' @return The object itself.
    setSeries = function(x) {
      # Check if there is existing data.
      if (length(self$series) > 1) {
        self$series <- TDI::colsbind.xts(self$series, x)
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
    #' @return Instrument returns time series.
    getReturn = function() {
      if (is.null(nrow(self$series))) {
        warning("Return not calculated due to missing data.")
        return(NULL)
      }
      return(self$series$Return)
    },

    #' @description
    #' Return the *wealth index* calculated from the returns.
    #' @return Instrument returns time series.
    getWealthIndex = function() {
      if (is.null(nrow(self$series))) {
        warning("Wealth index not calculated due to missing data.")
        return(NULL)
      }
      return(cumprod(self$series$Return + 1))
    },

    #' @description
    #' Execute the specified technical analysis calculations.
    #' @param ta Technical analysis indicator object.
    #' @return The object itself.
    TechnicalAnalysis = function(ta = NULL) {
      if (is.null(nrow(self$series))) {
        warning("Technical analysis not calculated due to missing data.")
      } else if (is.TAIndicator(ta)) {
        res <- ta$calculate(self$series)
        self$setSeries(res)
      } else {
        warning("Technical analysis indicator not valid.")
      }

      invisible(self)
    }

  )
)

