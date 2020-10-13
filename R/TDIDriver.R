#' @title Check for class TDIDriver
#' @description 
#' Helper to check the object class.
#' If the input `x` is empty it returns the class name.
#' @param x Object to check.
#' @return Boolean result or alternative the class name.
#' @export
is.TDIDriver <- function(x = NULL) {
  class = "TDIDriver"
  if (is.null(x)) { return(class) 
  } else return(inherits(x, class))
}

#' @title TDIDriver (R6 class constructor)
#' @description 
#' Generic and abstract R6 class for all TDI drivers (e.g., Yahoo, IEX, etc).
#' Inherit from this class to implement the API specific driver.
#' 
#' @details
#' The abstract class `TDIDriver` is the generic class for API specific driver(s).
#' Each API has a driver sub-class of `TDIDriver` class to initiate the API.
#' 
#' @import R6
#' @export
TDIDriver <- R6::R6Class(is.TDIDriver(), inherit = baseTDI,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  lock_class = TRUE, # lock the interface
  portable = TRUE, # enable inheritance across packages
  
  # Generic public methods for drivers.
  public = list(
    #' @description 
    #' Initialize driver object and validate the sub-class implementation.
    #' @return An object of class `TDIDriver`.
    initialize = function() { 
      # Validate sub-class connect method implementation.
      stopifnot(is.function(self$connect))
      stopifnot(is.null(formals(self$connect)))
      invisible(self) 
    }
  )
)

#' @title Create driver for the source API
#' @description 
#' Generic helper method to create a driver object to access the source API.
#' @param source Source code to identify the API.
#' @import R6
#' @export
driver <- function(source) {
  stopifnot(is.String(source))
  
  # Try to establish the connection.  
  tryCatch({
    class <- eval(parse(text = source))
    stopifnot(R6::is.R6Class(class))
    do.call(class$new, args = list())
  }, error = function(e) {
    message("Driver failed to setup API connection: ", source, ".")
    message(e)
    return(NULL)
  })
}