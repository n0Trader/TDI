#' @title Check for base class of TDI
#' @description 
#' Helper to check the object class.
#' If the input `x` is empty it returns the class name.
#' @param x Object to check.
#' @return Boolean result or alternative the class name.
#' @export
is.baseTDI <- function(x = NULL) {
  class = "baseTDI"
  if (is.null(x)) { return(class) 
  } else return(inherits(x, class))
}

#' @title TDI Base (R6 class constructor)
#' @description 
#' Generic and abstract R6 class within the TDI class structure.
#' @seealso Inspired by \href{https://dbi.r-dbi.org/}{DBI}
#' @import R6
baseTDI <- R6::R6Class(is.baseTDI(),
  class = TRUE, # enabled S3 classes
  cloneable = FALSE, 
  
  public = list(
    #' @description 
    #' Abstract class initialization generates an error.
    initialize = function() {
      stop("This base class is abstract and can't be initialized.")
    },
    
    #' @description
    #' Return the properties of this object as a list.
    #' @return List of object properties.
    fields = function() {
      f <- sapply(self, function(x) {
        # Skip function(s) and environment(s).
        if (is.function(x)) { return()
        } else if (is.environment(x)) { 
          if (is.baseTDI(x)) return(list(x$fields()))
          else return()
        } else { return(x) }
      }, USE.NAMES = TRUE)
      return(as.list(do.call(c, f)))
    }
  )
)