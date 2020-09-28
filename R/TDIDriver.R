#' @title TDIDriver class
#' @description
#' Base class for all TDI drivers (e.g., Yahoo, IEX, etc).
#' The virtual class `TDIDriver` is the generic class for API specific driver(s).
#' Each API has a driver sub-class of `TDIDriver` class to initiate the API.
#'
#' @docType class
#' @name TDIDriver-class
#' @family TDI classes
#' @family TDIDriver generics
#' @include TDIObject.R
#' @export
setClass("TDIDriver", contains = c("TDIObject", "VIRTUAL"))

#' @title Create driver for the source API.
#' @description 
#' Generic helper method to create a driver object to access the source API.
#' @import methods
#' @param source Source code for the API.
#' @export
driver <- function(source) {
  stopifnot(nchar(source) > 0)
  if (methods::isClass(source)) { methods::new(source)
  } else stop(paste0("Driver class could not be found for ", source, "."), call. = TRUE)
}

#' @title Provide API connection
#' @description 
#' Retrieves information on objects of classes inheriting from [TDIObject-class].
#' @docType methods
#' @param obj An object of class `TDIDriver`.
#' @param ... Other arguments.
#' @family TDIDriver generics
setGeneric("apiConnect",
  def = function(obj, ...) standardGeneric("apiConnect")
)

#' @rdname TDIDriver-class
#' @param obj An object of class `TDIDriver`.
setMethod("getInfo", "TDIDriver", function(obj) {
  return(class(obj))
})