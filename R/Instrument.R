#' @title Check if the object is an instrument.
#' @description 
#' This helper function checks if the object is of class Instrument.
#' If the input `x` is empty it returns the class name.
#' @param x Object to check the class for.
#' @return Boolean result for the class check or alternative the class name.
#' @export
is.Instrument <- function(x = NULL) {
  class = "Instrument"
  if (is.null(x)) { return(class) 
  } else return(inherits(x, class))
}

#' @title Instrument class
#' @description
#' Instrument is a sub-class of `TDIResult` for all types of (tradeable) market instruments.
#' @docType class
#' @name Instrument-class
#' @family TDI classes
#' @include TDIResult.R
#' @export
setClass("Instrument", contains = c("TDIResult"),
  slots = list(
    #' @slot .currency Nomination currency.
    ".currency" = "_Char", 
    #' @slot .type Type classification.
    ".type" = "_Char" 
  )
)

#' @title Instrument constructor
#' @description 
#' Helper constructor for object(s) of class `Instrument`.
#' @include utils.R
#' @param symbol Symbol to identify the instrument.
#' @param source Source for instrument data.
#' @param currency Denomination currency.
#' @param type Type of instrument.
#' @return Object of class `Instrument`.
#' @export
Instrument <- function(symbol, source, currency = NULL, type = NULL) {
  stopifnot(is.String(symbol))
  stopifnot(is.String(source))
  
  methods::new(is.Instrument(),
    .sources = list(source),
    .symbol = symbol,
    .currency = currency,
    .type = type
  )
}

#' @import xts
#' @import zoo
#' @rdname setSeries
setMethod("setSeries", signature("Instrument"), function(obj, x) {
  obj <- callNextMethod()
  if (has.Cl(obj@.series)) {
    close <- Cl(obj@.series)
    obj@.series$Return <- zoo::na.fill((close - xts::lag.xts(close))/xts::lag.xts(close), 0)
  }  
  invisible(obj)
})

#' @title Get trading session
#' @description 
#' Return the tradung session data for the input date plus `n` periods.
#' @docType methods
#' @family Instrument generics
#' @param obj An object of class `Instrument`.
#' @param date Input date-time.
#' @param n Optional lag number of sessions.
#' @return Requested session data.
#' @export
setGeneric("getSession", 
  def = function(obj, date, n = 0) standardGeneric("getSession")
)
#' @rdname getSession
setMethod("getSession", signature("Instrument"), function(obj, date, n) {
  i <- obj@.series[date, which.i = TRUE] + n
  if ((i > nrow(obj@.series)) || (identical(i, numeric(0)))) return(NULL)
  else return(obj@.series[i])
})

#' @title Get instrument return
#' @description 
#' Return the object *returns* calculated from the `Close` price(s).
#' @docType methods
#' @param obj An object of class `Instrument`.
#' @return Instrument return series.
#' @export
setGeneric("getReturn", 
  def = function(obj) standardGeneric("getReturn")
)
#' @rdname getReturn
setMethod("getReturn", signature("Instrument"), function(obj) {
  if (is.null(nrow(obj@.series))) {
    warning("Return not calculated, missing data.")
    return(NULL)
  }
  return(obj@.series$Return)
})

#' @title Get instrument wealth index
#' @description 
#' Return the object wealth index calculated from the returns.
#' @docType methods
#' @param obj An object of class `Instrument`.
#' @return Instrument wealth index series.
#' @export
setGeneric("getWealthIndex", 
  def = function(obj) standardGeneric("getWealthIndex")
)
#' @rdname getWealthIndex
setMethod("getWealthIndex", signature("Instrument"), function(obj) {
  if (is.null(nrow(obj@.series))) {
    warning("Wealth index not calculated, missing data.")
    return(NULL)
  }
  return(cumprod(obj@.series$Return + 1))
})
