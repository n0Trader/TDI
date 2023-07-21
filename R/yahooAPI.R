#' @title Yahoo Finance API
#' @description
#' Yahoo Finance API class inherits `TDIConnection` to implement API requests.
#' @details
#' Yahoo Finance API provides a wide range of data that can be accessed via the below URL.
#' The data returned is specified by the requested modules in the query string.
#'
#' https://query2.finance.yahoo.com/v10/finance/quoteSummary/MSFT?modules=assetProfile%2CsummaryProfile
#'
#' Available modules:
#' - 'assetProfile',
#' - 'summaryProfile',
#' - 'summaryDetail',
#' - 'esgScores',
#' - 'price',
#' - 'incomeStatementHistory',
#' - 'incomeStatementHistoryQuarterly',
#' - 'balanceSheetHistory',
#' - 'balanceSheetHistoryQuarterly',
#' - 'cashflowStatementHistory',
#' - 'cashflowStatementHistoryQuarterly',
#' - 'defaultKeyStatistics',
#' - 'financialData',
#' - 'calendarEvents',
#' - 'secFilings',
#' - 'recommendationTrend',
#' - 'upgradeDowngradeHistory',
#' - 'institutionOwnership',
#' - 'fundOwnership',
#' - 'majorDirectHolders',
#' - 'majorHoldersBreakdown',
#' - 'insiderTransactions',
#' - 'insiderHolders',
#' - 'netSharePurchaseActivity',
#' - 'earnings',
#' - 'earningsHistory',
#' - 'earningsTrend',
#' - 'industryTrend',
#' - 'indexTrend',
#' - 'sectorTrend'
#'
#' @import R6
#' @export
yahooAPI <- R6::R6Class("yahooAPI", inherit = TDIConnection,
  cloneable = FALSE, class = TRUE, # enabled S3 classes

  # Implement API endpoints.
  public = list(
    #' @description
    #' Retrieve instrument details for the symbol.
    #' @param ... see \code{\link{TDIConnection}}
    #' @return An object of class `Instrument` with details.
    getInstrument = function(...) {
      # Super returns a list with the function arguments.
      args <- super$getInstrument(...)

      # Try to execute the Json API request.
      tryCatch({
        res <- private$requestString(
            endpoint = "details",
            path = list(args$symbol)
        ) %>% private$jsonRequest()

        # Call the handler for the result
        return(private$quoteSummaryHandler(args, res))

      }, error = function(e) {
        msg <- conditionMessage(e)
        warning(msg, call. = TRUE)
      })
    },

    #' @description
    #' Retrieve cash flow data for the symbol.
    #' @param ... see \code{\link{TDIConnection}}
    #' @return An object of class `Instrument` with details.
    getCashFlow = function(...) {
      # Super returns a list with the function arguments.
      args <- super$getCashFlow(...)

      # Try to execute the Json API request.
      tryCatch({
        res <- private$requestString(
            endpoint = "cashflow",
            path = list(args$symbol)
        ) %>% private$jsonRequest()

        # Call the handler for the result
        return(private$quoteSummaryHandler(args, res))

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

      # Check if the API query is by period or by range.
      if (!is.null(args$from)) {
        # Query parameters with series period by dates.
        params <- list(
          "period1" = convertDate2Unix(args$from),
          "period2" = convertDate2Unix(defaultToday(args$to)),
          "interval" = private$validateParam(interval = args$interval),
          "includeTimestamps" = TRUE
        )
      } else {
        # Query parameters with series range.
        params <- list(
          "range" = private$validateParam(range = args$range),
          "interval" = private$validateParam(interval = args$interval),
          "includeTimestamps" = TRUE
        )
      }

      # Try to execute the Json API request.
      tryCatch({
        res <- private$requestString(
          endpoint = "chart", path = list(args$symbol), params
        ) %>% private$jsonRequest()

        private$validateResponse(res, "chart")
        if (is.null(res$chart$error)) {
          # Convert the results into a new instrument object.
          instr <- yahooInstrument$new(
            source = as.character(class(self$driver)[1]),
            symbol = as.character(args$symbol)
          )

          # Add historical prices to the instrument.
          instr$type = res$chart$result$meta$instrumentType
          instr <- instr$setSeries(data.frame(
            Date = convertUnix2Date(res$chart$result$timestamp[[1]]),
            Open = res$chart$result$indicators$quote[[1]]$open[[1]],
            High = res$chart$result$indicators$quote[[1]]$high[[1]],
            Low = res$chart$result$indicators$quote[[1]]$low[[1]],
            Close = res$chart$result$indicators$quote[[1]]$close[[1]],
            Volume = res$chart$result$indicators$quote[[1]]$volume[[1]]
          ))
          return(instr)

        } else {
          stop(paste(res$quoteSummary$error$description, symbol, sep = ": "))
        }

      }, error = function(e) {
        msg <- conditionMessage(e)
        warning(msg, call. = TRUE)
      })
    }
  ),

  # Private API handlers.
  private = list(
    # Handle JSON response for quoteSummary.
    quoteSummaryHandler = function(args, res) {
      private$validateResponse(res, "quoteSummary")
      if (!is.null(res$quoteSummary$error)) {
        symbol <- as.character(args$symbol)
        stop(paste(res$quoteSummary$error$description, symbol, sep = ": "))
      } else {
        # Convert the results into a new instrument object.
        yahooInstrument$new(
          source = as.character(class(self$driver)[1]),
          symbol = as.character(args$symbol),
          data = res$quoteSummary$result
        ) %>% return()
      }
    }
  )
)
