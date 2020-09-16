#' @title TDIResult class
#' @description
#' Base class for all TDI result(s) sub-classes (e.g. Instrument, Index).
#' The virtual class `TDIResult` is the generic class for specific result(s) classes.
#' Each type of results implements a specific [TDIResult-class].
#' 
#' @docType class
#' @name TDIResult-class
#' @family TDI classes
#' @family TDIResult generics
#' @include TDIObject.R
#' @export
setClass("TDIResult", contains = c("TDIObject", "VIRTUAL"))