#' @title TDIDriver class
#' @description
#' Base class for all TDI drivers (e.g., Yahoo, IEX, etc).
#' The virtual class `TDIDriver` is the generic class for API specific driver(s).
#' 
#' Each API implements an inherited [TDIDriver-class] to create the API driver object.
#' Object(s) of classes inheriting from [TDIDriver-class] to access the API.
#'
#' @docType class
#' @name TDIDriver-class
#' @family TDI classes
#' @family TDIDriver generics
#' @export
#' @include TDIObject.R
setClass("TDIDriver", contains = c("TDIObject", "VIRTUAL"))

#' @title Create driver for the source API.
#' @description 
#' Generic helper method to create a driver object to access the source API.
#' @export
driver <- function(source) {
  stopifnot(nchar(source) > 0)
  if (isClass(source)) { new(source)
  } else stop(paste0("Driver class could not be found for ", source, "."), call. = TRUE)
}

#' @title Provide API connection
#' Retrieves information on objects of classes inheriting from [TDIObject-class].
#'
#' @param obj An object of [TDIDriver-class],
#' @param ... Other arguments.
#' @family TDIDriver generics
#' @inherit TDItest::spec_get_info return
#' @export
setGeneric("apiConnect",
  def = function(obj, ...) standardGeneric("apiConnect")
)

#' @rdname TDIDriver-class
#' @export
setMethod("getInfo", "yahoo", function(obj, ...) {
  return(class(obj))
})