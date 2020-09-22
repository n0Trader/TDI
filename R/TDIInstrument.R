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
    #' @slot .type Type classification.
    ".type" = "character" 
  )
)
