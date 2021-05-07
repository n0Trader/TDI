#' Generic API error handling helper.
#' @noRd
.errorHandler <- function(e) {
  message(e)
  return(conditionMessage(e))
}

#' Echo that we are online.
#' @description 
#' Return a message that the API is online.
#' 
#* @get /echo
function() {
  list(msg = "The API is online.")
}

#' R version information.
#' @description 
#' Return a message with R version information.
#' 
#* @get /info
function() {
  list(msg = R.Version())
}

#' Instrument summary data.
#' @description 
#' Return the instrument summary data.
#' 
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

#' Instrument chart data.
#' @description 
#' Return the instrument chart data.
#' 
#* @param symbol Instrument symbol.
#* @param source Data source.
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

#' Instrument cash flow data.
#' @description 
#' Return the instrument cash flow data.
#' 
#* @param symbol Instrument symbol.
#* @param source Data source.
#* @return Instrument cash flow data.
#* 
#* @serializer unboxedJSON
#* @get /cashflow
function(symbol, source, range = "1y") {
  con <- TDIConnector$connect(tolower(source))
  ins <- con$getCashFlow(toupper(symbol))
  return(ins$fields())
}
