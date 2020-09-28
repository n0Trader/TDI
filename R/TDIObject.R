#' @title TDIObject class
#' @description
#' Base class for all TDI classes (e.g., drivers, etc). This
#' is a virtual Class: No objects may be created from it.
#'
#' More generally, the TDI defines a small set of classes and generics to
#' facilitate a common interface for trading data.
#'
#' @seealso Inspired by \href{https://dbi.r-dbi.org/}{DBI}
#' @docType class
#' @name TDIObject-class
#' @family TDI classes
#' @export
setClass("TDIObject", "VIRTUAL")

#' @title Get metadata
#' @description
#' Retrieves information on objects of classes inheriting from [TDIObject-class].
#' @docType methods
#' @family TDIDriver generics
#' @param obj An object of class `TDIObject`.
#' @param ... Other arguments.
setGeneric("getInfo",
  def = function(obj, ...) standardGeneric("getInfo")
)
