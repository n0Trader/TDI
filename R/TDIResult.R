#' @title TDIResult (R6 class constructor)
#' @description 
#' Generic base class for TDI result(s) sub-classes (e.g. TDIInstrument).
#' @import R6
#' @import xts
#' @export
TDIResult <- R6::R6Class("TDIResult", inherit = baseTDI,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  portable = TRUE, # enable inheritance across packages
  
  # Implement TDI result generic methods.
  public = list(
    #' @field sources Identifier for data source.
    sources = as.character(),
    #' @field symbol Unique identification symbol.
    symbol = as.character(),
    #' @field series Xts time-series with (historical) data.
    series = NA,
    
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
    }
  )
)