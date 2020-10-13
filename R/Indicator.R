#' @title Check if the object is of class Indicator
#' @description 
#' Helper to check the object class.
#' If the input `x` is empty it returns the class name.
#' @param x Object to check.
#' @return Boolean result or alternative the class name.
#' @export
is.Indicator <- function(x = NULL) {
  class = "Indicator"
  if (is.null(x)) { return(class) 
  } else return(inherits(x, class))
}

#' @title Indicator (R6 class constructor)
#' @description 
#' Indicator is a R6 class for economic data and other indicator(s).
#' @import R6
#' @export
Indicator <- R6::R6Class(is.Indicator(), inherit = TDIResult,
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  # Extend the TDI result fields.
  public = list(
    #' @field type Indicator type classification.
    type = as.character(),
    
    #' @description 
    #' Constructor for object(s) of class `Indicator`.
    #' @param source API source for the data.
    #' @param symbol Unique identification symbol.
    #' @param type Indicator type classification.
    #' @return An object of class `Indicator`.
    initialize = function(source, symbol, type) {
      stopifnot(is.String(source))
      stopifnot(is.String(symbol))
      stopifnot(is.String(type))
      
      # Initialize the object.
      self$sources <- list(source)
      self$symbol <- symbol
      self$type <- type
      invisible(self)
    }
  )
)
