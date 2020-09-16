#' Trading Data Interface (TDI)
#' 
#' Implement TDI following concept from RDI.
#'
#' Main classes:
#' - TDIObject, a common base class
#' - TDIDriver, the instrument data source/driver
#' - TDIResult, results returned by API(s)
#' - TDIInstrument, represents generic instrument
#' - TDIIndex, represents specific instrument
#' - TDI<type>, specific instrument types
#' - TDIExchange, represent the exchange
#' 


#' @title TDIObject class
#' @description
#' Base class for all TDI classes (e.g., drivers, etc). This
#' is a virtual Class: No objects may be created from it.
#'
#' More generally, the TDI defines a small set of classes and generics to
#' facilitate a common interface for trading data.
#'
#' @seealso Inspired by \link[DBI]{DBIObject}
#' @docType class
#' @name TDIObject-class
#' @family TDI classes
#' @export
setClass("TDIObject", "VIRTUAL")

#' @title Get metadata
#' Retrieves information on objects of classes inheriting from [TDIObject-class].
#'
#' @param obj An object of [TDIObject-class],
#' @param ... Other arguments.
#' @family TDIDriver generics
#' @inherit TDItest::spec_get_info return
setGeneric("getInfo",
  def = function(obj, ...) standardGeneric("getInfo")
)
