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
  class = "TDIInstrument"
  if (is.null(x)) { return(class) 
  } else return(inherits(x, class))
}

#' @title TDIInstrument class
#' @description
#' Instrument class is a [TDIResult-class] for all types of (tradeable) market instruments.
#' 
#' @docType class
#' @name TDIInstrument-class
#' @family TDI classes
#' @include TDIResult.R
#' @export
setClass("TDIInstrument", contains = c("TDIResult"),
  slots = list(
    #' @slot .currency Nomination currency.
    ".currency" = "character", 
    #' @slot .type Type classification.
    ".type" = "character" 
  )
)

#' @title Instrument constructor
#' @description 
#' Helper constructor for object(s) of class TDIInstrument.
#' @param symbol Symbol to identify the instrument.
#' @param sources Sources for instrument data.
#' @return Object of class TDIInstrument.
#' @export
Instrument <- function(symbol, source) {
  stopifnot(is.String(symbol))
  stopifnot(is.String(source))
  
  methods::new(is.Instrument(),
    .sources = list(source),
    .symbol = symbol
  )
}