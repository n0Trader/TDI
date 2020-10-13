#' @title TDIResult (R6 class constructor)
#' @description 
#' Generic base class for TDI result(s) sub-classes (e.g. Instrument, Index).
#' @import R6
#' @import xts
#' @export
TDIResult <- R6::R6Class("TDIResult", inherit = baseTDI,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  portable = TRUE, # enable inheritance across packages
  
  # Implement TDI result generic methods.
  public = list(
    #' @field sources Named list of sources for the data.
    sources = list(),
    #' @field symbol Unique identification symbol.
    symbol = as.character(),
    #' @field series Xts time-series with (historical) data.
    series = NULL,
    
    #' @description 
    #' Set series or add additional column(s) to the existing data.
    #' In case the input is a data-frame it is converted to `xts`.
    #' @param x Xts time-series with (historical) data.
    #' @return An object of class `TDIResult`.
    setSeries = function(x) {
      # Check if there is existing data.
      if (length(self$series) > 0) {
        stopifnot(xts::is.xts(x))
        
        # Mark duplicates to be dropped.
        drop <- which(colnames(self$series) %in% colnames(x))
        if (length(drop) == length(colnames(self$series))) {
          # Drop all means we can replace X with Y.
          self$series <- x
        } else if (length(drop) > 0) {
          # Merge y with x minus dropped column(s).
          self$series <- cbind(self$series[, -drop], x)
        } else {
          # No duplicates to drop.
          self$series <- cbind(self$series, x)
        }
        
      } else if (is.data.frame(x)) {
        self$series <- zoo::na.locf(xts::as.xts(x[,-1], order.by = x$Date))
      } else if (xts::is.xts(x)) {
        self$series <- zoo::na.locf(x)
      } else stop("Invalid data structure.", call. = TRUE)
      invisible(self)
    }
    
  )
)