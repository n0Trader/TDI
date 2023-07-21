#' Generic API error handling helper.
#' @noRd
.errorHandler <- function(e) {
  message(e)
  return(conditionMessage(e))
}

#* Return a message that the API is online.
#* @get /echo
function() {
  return("The API is online.")
}

#* Return a message with R version information.
#* @get /info
function() {
  list(msg = R.Version())
}

#* For future use to manage session data.
#* @filter connection
function(req, source = NULL) {
  message("TDI request: ", req$PATH_INFO, req$QUERY_STRING)
  message("Content-type: ", req$HTTP_CONTENT_TYPE)
  message("Body: ", req$bodyRaw)
  plumber::forward()
}

#* GET instrument summary data from specified source.
#*
#* @param symbol Instrument symbol.
#* @param source Data source.
#* @return Instrument summary data.
#* @serializer unboxedJSON
#* @get /instrument
function(symbol, source) {
  con <- TDIConnector$connect(tolower(source))
  ins <- con$getInstrument(toupper(symbol))
  return(ins$fields())
}

#* GET instrument chart data from specified source.
#*
#* @param symbol Instrument symbol.
#* @param source Data source to query.
#* @param range Optional range of historical data.
#* @return Instrument chart data.
#*
#* @serializer json
#* @get /chart
function(symbol, source, range = "1y") {
  con <- TDIConnector$connect(tolower(source))
  ins <- con$getChart(toupper(symbol), range = range)
  return(as.data.frame(ins$series))
}

#* POST symbols with source to retrieve quotes.
#*
#* @return Quotes for instrument(s).
#* @serializer unboxedJSON
#* @post /quotes
function(req) {
  symbols <- rawToChar(req$bodyRaw) %>%
    jsonlite::fromJSON()

  quotes <- apply(symbols, 1, function(x) {
    con <- TDIConnector$connect(tolower(x["source"]))
    ins <- con$getInstrument(toupper(x["symbol"]))
    if (is.Instrument(ins)) {
      return(ins$quote)
    }
  })

  result <- cbind(
    symbol = symbols$symbol,
    source = symbols$source,
    quote = quotes
  )

  return(data.frame(result))
}

#* POST instrument technical analysis.
#*
#* @param symbol Instrument symbol.
#* @param source Data source to query.
#* @param range Optional range of historical data.
#* @return Instrument technical analysis data.
#*
#* @serializer json
#* @post /chart
function(req, symbol, source, range = "1y") {
  con <- TDIConnector$connect(tolower(source))
  ins <- con$getChart(toupper(symbol), range = range)
  ind <- jsonlite::fromJSON(req$body)
  apply(ind, 1, function(x) {
    ta <- TDI::taFactory(x)
    ins <- ins$TechnicalAnalysis(ta)
  })
  return(as.data.frame(ins$series))
}

#* GET instrument cash flow data from specified source.
#*
#* @param symbol Instrument symbol.
#* @param source Data source to query.
#* @return Instrument cash flow data.
#*
#* @serializer unboxedJSON
#* @get /cashflow
function(symbol, source) {
  con <- TDIConnector$connect(tolower(source))
  ins <- con$getCashFlow(toupper(symbol))
  return(ins$fields())
}
