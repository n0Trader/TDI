#' @title Borsdata API (R6 constructor class)
#' @description
#' Borsdata API class inherits `TDIConnection` to implement the API requests.
#' @seealso API documentation on \strong{\href{https://github.com/Borsdata-Sweden/API}{Börsdata API}}.
#' @import R6
#' @export
bdataAPI <- R6::R6Class("bdataAPI", inherit = TDIConnection,
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  public = list(
    #' @field countries Data frame with list of countries.
    countries = NULL,
    #' @field sectors Data frame with list of sectors.
    sectors = NULL,
    #' @field branches Data frame with list of branches.
    branches = NULL,
    #' @field markets Data frame with list of markets.
    markets = NULL,
    #' @field instruments Data frame with list of instruments.
    instruments = NULL,

    #' @description
    #' !!!!!
    #' @param ... see \code{\link{TDIConnection}}
    #' @return An object of class `bdataAPI`.
    initialize = function(...) {
      args <- super$initialize(...)

      # Update API meta data.
      self$countries <- private$fetchMetaData("countries")
      self$sectors <- private$fetchMetaData("sectors")
      self$branches <- private$fetchMetaData("branches")
      self$markets <- private$fetchMetaData("markets")
      self$instruments <- private$fetchMetaData("instruments")
      invisible(self)
    }

  ),

  private = list(
    # Call the API to fetch meta data
    fetchMetaData = function(metadata) {
      # Execute the Json API request with URL request string.
      res <- private$jsonRequest(
        private$requestString(endpoint = metadata, path = NULL,
        query = list(authKey = as.character(self$conn_args$api_token))
      ))
      private$validateResponse(res, metadata)
      message("Fetching metadata: ", nrow(res[[metadata]]), " ", metadata, ".")
      return(res[[metadata]])
    }
  )
)
