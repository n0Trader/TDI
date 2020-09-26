#' @title Check if the object is an instrument.
#' @description 
#' This helper function checks if the object is of class Instrument.
#' If the input `x` is empty it returns the class name.
#' 
#' @param x Object to check the class for.
#' @return Boolean result for the class check or alternative the class name.
#' @export
#' @noRd
is.Instrument <- function(x = NULL) {
  class = "Instrument"
  if (is.null(x)) { return(class) 
  } else return(inherits(x, class))
}

#' @title Instrument class
#' @description
#' Instrument class is a [TDIResult-class] for all types of (tradeable) market instruments.
#' 
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
#' Helper constructor for object(s) of class Instrument.
#' @param symbol Symbol to identify the instrument.
#' @param source Source for instrument data.
#' @param currency Denomination currency.
#' @param type Type of instrument.
#' @return Object of class Instrument.
#' @include utils.R
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

#' @rdname Instrument-class
#' @import xts
#' @import zoo
setMethod("setSeries", signature("Instrument"), function(obj, df) {
  obj <- callNextMethod()
  if (has.Cl(obj@.series)) {
    close <- Cl(obj@.series)
    obj@.series$Return <- zoo::na.fill((close - xts::lag.xts(close))/xts::lag.xts(close), 0)
  }  
  invisible(obj)
})

#' @title Get trading session
#' @family Instrument generics
#' @param obj An object of [Instrument-class].
#' @param date Input date-time.
#' @param n Optional lag number of sessions.
#' @return Session for date plus n.
#' @export
setGeneric("getSession", 
  def = function(obj, ...) standardGeneric("getSession")
)
setMethod("getSession", signature("Instrument"), function(obj, date, n = 0) {
  return(obj@.series[obj@.series[date, which.i = TRUE] + n])
})

#' @title Get instrument return
#' @param obj An object of [TDIResult-class].
#' @return Instrument return series.
#' @export
setGeneric("getReturn", 
  def = function(obj) standardGeneric("getReturn")
)
setMethod("getReturn", signature("Instrument"), function(obj) {
  if (is.null(nrow(obj@.series))) {
    warning("Return not calculated, missing data.")
    return(NULL)
  }
  return(obj@.series$return)
})

#' @title Get instrument wealth index
#' @param obj An object of [TDIResult-class].
#' @return Instrument wealth index series.
#' @export
setGeneric("getWealthIndex", 
  def = function(obj) standardGeneric("getWealthIndex")
)
setMethod("getWealthIndex", signature("Instrument"), function(obj) {
  if (is.null(nrow(obj@.series))) {
    warning("Wealth index not calculated, missing data.")
    return(NULL)
  }
  return(cumprod(obj@.series$return + 1))
})
