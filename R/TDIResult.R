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
    #' @param prop Optional property in recursive call.
    #' @return List of object properties.
    fields = function(prop = NULL) {
      if (missing(prop)) {
        # Recursive through properties and skip functions.
        fields <- list()
        for (name in names(self)) {
          prop <- get(name, envir = self)
          if (!is.function(prop) && !is.null(prop)) {
            fields[[name]] <- self$fields(prop)
          }
        }
        return(rev(fields))

      } else if (is.environment(prop)) {
        # Recursive call TDI base objects and skip other environments.
        if (is.baseTDI(prop)) {
          return(prop$fields())
        } else return()

      } else if (is.list(prop)) {
        res <- list()
        for (i in 1:length(prop)) {
          res[[i]] <- self$fields(prop[[i]])
        }
        return(res)

      } else if (is.na(prop)) { return(NULL)
      } else return(prop)
    },

    #' @description
    #' Validate the properties of the object.
    #' It is assumed that all NA properties are optional.
    #' @param prop Optional property in recursive call.
    #' @return Vector with validation result.
    validate = function(prop) {
      if (missing(prop)) {
        f <- sapply(self, function(x) {
          if (is.function(x)) { return()
          } else return(self$validate(x))
        }, USE.NAMES = TRUE, simplify = TRUE)
        return(unlist(do.call(c, f)))

      } else if (is.environment(prop)) {
        # Recursive call TDI base objects and skip other environments.
        if (is.baseTDI(prop)) {
          return(prop$validate())
        } else return()

      } else if (is.list(prop)) {
        res <- list()
        for (i in 1:length(prop)) {
          res[[i]] <- self$validate(prop[[i]])
        }
        return(res)

      } else if (is.null(prop)) { return(FALSE)
      } else if (is.na(prop)) { return(NULL)
      } else return(TRUE)
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
