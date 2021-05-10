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
  plumber::forward()
}

#* GET instrument summary data from specified source.
#*
#* @param symbol Instrument symbol.
#* @param source Data source.
#* @return Instrument summary data.
#*
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
