#' @title TDIResult (R6 class constructor)
#' @description 
#' Generic base class for TDI result(s) sub-classes (e.g. TDIInstrument).
#' @details 
#' The TDI result(s) base class provides generic methods for results.
#' Validation methods allow for a quick check of API implementation classes.
#' These are recommended to be included in the tests for each API.
#' The validation check assumes that properties with NA are optional.
#' @import R6
#' @import xts
#' @export
TDIResult <- R6::R6Class("TDIResult", inherit = baseTDI,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  portable = TRUE, # enable inheritance across packages
  
  # Implement TDI result generic methods.
  public = list(
    #' @description
    #' Return the properties of the object as a list.
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
    },
    
    #' @description
    #' Validate the properties of the object.
    #' It is assumed that all NA properties are optional.
    #' @return Vector with validation result.
    validate = function() {
      f <- sapply(self, function(x) {
        # Skip function(s) and environment(s).
        if (is.function(x)) { return()
        } else if (is.environment(x)) { 
          if (is.baseTDI(x)) return(x$validate())
          else return()
        } else if (is.null(x)) { return(FALSE)
        } else return(TRUE)
      }, USE.NAMES = TRUE)
      return(do.call(c, f))
    },

    #' @description
    #' Check if the object properties are valid.
    #' A simple validation to check API implementation.
    #' @return Boolean as validation result.
    isValid = function() {
      result <- self$validate()
      if (!isTRUE(all(result))) { 
        invalid <- result[!result]
        warning("Failed validation: ", paste(names(invalid), invalid, sep = ":", collapse = ", "), ".")
      }
      return(isTRUE(all(result)))
    }
    
  )
)
