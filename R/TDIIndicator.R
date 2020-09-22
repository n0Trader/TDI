#' @title TDIIndicator class
#' @description
#' Indicator class is a [TDIResult-class] for economic data and other indicator(s).
#' 
#' @docType class
#' @name TDIIndicator-class
#' @family TDI classes
#' @include TDIResult.R
#' @export
setClass("TDIIndicator", contains = c("TDIResult"),
  slots = list(
    #' @slot .type Type classification.
    ".type" = "character" 
  )
)
