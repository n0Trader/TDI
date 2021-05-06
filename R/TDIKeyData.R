#' @title TDIKeyData (R6 class constructor)
#' @description 
#' Generic abstract base class for TDI result(s) key data.
#' The class is a data structure for instrument key data,
#' and should be implemented in the API implementation.
#' Using R6 classes for data structures enforce standardization.
#' @import R6
#' @export
TDIKeyData <- R6::R6Class("TDIKeyData", inherit = TDIResult,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  portable = TRUE, # enable inheritance across packages
  
  # Define data structure.
  public = list(
    #' @field sharesOutstanding Total number of shares.
    sharesOutstanding = as.numeric(),
    #' @field price Current price.
    price = as.numeric(),
    #' @field marketCap Shares total market capitalization.
    marketCap = as.numeric(),
    #' @field ebitda EBITDA.
    ebitda = as.numeric(),
    #' @field trailingEPS Trailing earnings per share.
    trailingEPS = as.numeric(),
    #' @field forwardEPS Forward earnings per share.
    forwardEPS = as.numeric(),
    #' @field trailingPE Trailing price/earnings.
    trailingPE = as.numeric(),
    #' @field forwardPE Forward price/earnings.
    forwardPE = as.numeric(),
    #' @field PEG Price/earnings to growth ratio.
    PEG = as.numeric()
  )
)