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
    #' @field prices Latest instrument prices.
    prices = NULL,

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
    },

    #' @description
    #' Retrieve instrument details for the symbol.
    #' @param ... see \code{\link{TDIConnection}}
    #' @return An object of class `Instrument` with details.
    getInstrument = function(...) {
      # Super returns a list with the function arguments.
      args <- super$getInstrument(...)

      # Try to create an instrument object
      tryCatch({
        ins <- private$instrumentFactory(symbol = args$symbol)
        prices <- private$instrumentPrices(ins$symbol)
        ins$setQuote(
          Open = prices$o,
          High = prices$h,
          Low = prices$l,
          Close = prices$c,
          Volume = prices$v
        )
        return(ins)

      }, error = function(e) {
        msg <- conditionMessage(e)
        warning(msg, call. = TRUE)
      })
    },

    #' @description
    #' Retrieve historical prices for the symbol.
    #' @param ... see \code{\link{TDIConnection}}
    #' @return An object of class `Instrument` with historical prices.
    getChart = function(...) {
      args <- super$getChart(...)

      # Query parameters with series period by dates.
      if (!is.null(args$from)) {
        query <- list(
          from = convertDate2Unix(args$from),
          to = convertDate2Unix(defaultToday(args$to))
        )
      } else { query <- list() }

      # Try to execute the Json API request.
      tryCatch({
        id <- private$findInstrumentId(args$symbol)

        res <- private$bdataRequest(
          endpoint = "chart",
          path = list(id),
          query = query
        ) %>% private$jsonRequest()

        private$validateResponse(res, "stockPricesList")

        return(res)

      }, error = function(e) {
        msg <- conditionMessage(e)
        warning(msg, call. = TRUE)
      })
    }
  ),

  private = list(
    # Latest price(s) update.
    .latestPrices = Sys.time(),

    # Call the API to fetch meta data
    fetchMetaData = function(metadata) {
      # Execute the Json API request with URL request string.
      res <- private$bdataRequest(endpoint = metadata) %>%
        private$jsonRequest()

      private$validateResponse(res, metadata)
      message("Fetching data: ", nrow(res[[metadata]]), " ", metadata, ".")
      return(res[[metadata]])
    },

    # Construct request string with API key
    bdataRequest = function(endpoint, path = NULL, query = list()) {
      query["authKey"] = as.character(self$conn_args$api_token)
      private$requestString(
        endpoint = endpoint,
        path = path,
        query = query
      ) %>% return()
    },

    # Helper to find the instrument id by symbol.
    findInstrumentId = function(symbol) {
      ins <- self$instruments[self$instruments$ticker == symbol,]
      if (nrow(ins) == 1) { return(ins$insId)
      } else { stop("Instrument not found: ", symbol, ".") }
    },

    # Helper to construct an instrument object.
    instrumentFactory = function(symbol) {
      ins <- self$instruments[self$instruments$ticker == symbol,]
      if (nrow(ins) == 1) {
        bdataInstrument$new(
          source = as.character(class(self$driver)[1]),
          symbol = as.character(symbol),
          name = ins$name,
          exchange = as.character(self$markets[ins$marketId,]$name),
          sector = as.character(self$sectors[ins$sectorId,]$name),
          industry = as.character(self$branches[ins$branchId,]$name),
          country = as.character(self$countries[ins$countryId,]$name)
        ) %>% return()
      }
    },

    # Helper to retrieve prices 8with hourly refresh).
    instrumentPrices = function(symbol) {
      insId <- private$findInstrumentId(symbol)
      if (Sys.time() > private$.latestPrices) {
        self$prices <- private$fetchMetaData("stockPricesList")
        private$.latestPrices <- Sys.time() + 3600
      }
      return(self$prices[self$prices$i == insId,])
    }
  )
)
