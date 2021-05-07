#' @title Cash flow data (R6 class constructor)
#' @description 
#' Generic abstract base class for TDI cash flow data.
#' @details
#' The class is a data structure for cash flow data,
#' and should be implemented in the API implementation.
#' Using R6 classes for data structures enforce standardization.
#' @import R6
#' @export
TDICashFlow <- R6::R6Class("TDICashFlow", inherit = TDIResult,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  portable = TRUE, # enable inheritance across packages
  
  # Define data structure.
  public = list(
    #' @field endDate Reporting end date.
    endDate = NULL,
    #' @field netIncome Net income.
    netIncome = NULL,
    #' @field operatingCashFlow Operating cash flow.
    operatingCashFlow = NULL,
    #' @field capitalExpenditures Capital expenditures.
    capitalExpenditures = NULL,
    #' @field freeCashFlow Free cash flow.
    freeCashFlow = NULL
  ),
  
)