#' @title Yahoo Key Data mapper (R6 class constructor)
#' @description 
#' Yahoo Finance API implementation to retrieve cash flow data.
#' This class is a simple data mapper.
#' @import R6
#' @export
yahooCashFlow <- R6::R6Class("yahooCashFlow", inherit = TDICashFlow,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  
  # Implementation with public initialization.
  public = list(
    #' @description 
    #' Initialization with Yahoo Finance cash flow data mapping. 
    #' @param data Subset of data returned by the API.
    #' @return An object of class `YahooCashFlow`.
    initialize = function(data) {
      # Set TDICashFlow fields.
      self$endDate <- data[["endDate.fmt"]]
      self$netIncome <- as.numeric(data["netIncome.raw"])
      self$operatingCashFlow <- as.numeric(data["totalCashFromOperatingActivities.raw"])
      self$capitalExpenditures <- as.numeric(data["capitalExpenditures.raw"])
      self$freeCashFlow = self$operatingCashFlow - self$capitalExpenditures
      invisible(self)
    }
  )
)