#' @title TDIResult class
#' @description
#' Base class for all TDI result(s) sub-classes (e.g. Instrument, Index).
#' The class `TDIResult` is the generic class for specific result(s) classes,
#' and implements its shared generic methods.
#' 
#' @docType class
#' @name TDIResult-class
#' @family TDI classes
#' @family TDIResult generics
#' @include TDIObject.R
#' @importFrom xts xts 
#' @export
setClass("TDIResult", contains = c("TDIObject"),
  slots = list(
    #' @slot .source Named list of sources for the data.
    ".sources" = "list", 
    #' @slot .symbol Unique identification symbol.
    ".symbol" = "character", 
    #' @slot .type Type of result (related to class).
    ".type" = "character", 
    #' @slot .series Time-series of class xts.
    ".series" = "ANY" # S3 class xts
  )
)