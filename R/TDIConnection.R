#' @title TDIConnection (R6 class constructor)
#' @description 
#' TDIConnection is a generic and abstract R6 class that wraps an object of class `TDIDriver` and connection parameters.
#' Its purpose is to establish a connection and to store all connection details.
#' Inherit from this class to implement API connections.
#' 
#' @details 
#' The `TDIConnection` class is the super class for each API to hold the connection.
#' Each API has a driver of class `TDIDriver` that can setup the API connection (of class `TDIConnection`).
#' 
#' @import R6
#' @import httr
#' @export
TDIConnection <- R6::R6Class("TDIConnection", inherit = baseTDI,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  lock_class = TRUE, # lock the interface
  portable = TRUE, # enable inheritance across packages
  
  public = list(
    #' @field driver An object of class `TDIDriver`.
    driver = NULL, 
    #' @field conn_args List with connection parameters.
    conn_args = list(), 
    #' @field endpoints List with available API endpoints.
    endpoints = list(), 
    #' @field values List collection with valid parameter values.
    values = list(), 
    
    #' @description 
    #' Initialize the API connection for the sub-class.
    #' This class is abstract and cannot be instantiated. 
    #' @param driver An object of class `TDIDriver`.
    #' @param conn_args List with connection parameters.
    #' @param endpoints List with available API endpoints.
    #' @param values List with allowed values for parameter(s).
    #' @return An object of class `TDIConnection`.
    initialize = function(driver, conn_args, endpoints, values) {
      # Validate the class structure and method(s).
      if (which(class(self) %in% "TDIConnection") == 1L) {
        stop("TDIConnection class is abstract class and can't be initialized.")
      } else {
        
      }
      
      # Validate the constructor parameters.
      stopifnot(is.TDIDriver(driver))
      stopifnot(is.list(conn_args))
      stopifnot(hasName(conn_args, "baseURL"))
      stopifnot(is.list(endpoints))
      stopifnot(is.list(values))
      
      # Initialize the object.
      self$driver <- driver
      self$conn_args <- conn_args
      self$endpoints <- endpoints
      self$values <- values
      invisible(self)
    },
    
    #' @description
    #' Validate the parameter value against API allowed values.
    #' Return the first value if the parameter value is invalid.
    #' @param ... Parameter value to validate.
    #' @return Valid parameter value.
    validValue = function(...) {
      tryCatch({
        arg <- list(...)[1] # only use the first argument
        values <- self$values[[names(arg)]] # get valid values
        if (is.null(arg[[1]])) return(values[1]) # missing set default
        else if (is.null(values)) return(arg[[1]]) # nothing to validate
        else if (arg[[1]] %in% values) return(arg[[1]]) # valid!
        else return(values[1]) # invalid set default
      }, error = function(e) {
        message("Could not validate the parameter: ", as.character(names(arg)), ".")
        message(e)
        return(arg[[1]])
      })
    },
    
    #' @description 
    #' Construct request URL string for the API call.
    #' @param endpoint Endpoint name for the API request.
    #' @param path List with path parameters.
    #' @param query Optional request query parameters.
    #' @return URL string for the API request.
    requestString = function(endpoint, path, query = NULL) {
      stopifnot(endpoint %in% names(self$endpoints))
      stopifnot(is.list(path))
      stopifnot(any(is.null(query), is.list(query)))
      
      # Construct relative path for the endpoint.
      tryCatch({
        path <- do.call(sprintf, c(self$endpoints[[endpoint]], path))
        return(httr::modify_url(self$conn_args$baseURL, path = path, query = query))
      }, error = function(e) {
        message(e)
        return(NULL)
      })
    },
    
    #' @description
    #' Execute API Json request.
    #' @param url URL for the request.
    #' @return Response content in Json format.
    jsonRequest = function(url) {
      # Call the URL and check the results.
      resp <- httr::GET(url)
      
      if (httr::status_code(resp) != 200) {
        warning(paste(url,
          paste0("API request failed with HTTP status: ", httr::status_code(resp)),
          sep = "\n"
        ))
      } else if (httr::http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
      }
      
      # Parse the JSON results and check the results.
      resp <- try(jsonlite::fromJSON(
        httr::content(resp, "text", encoding = "UTF-8")), 
        silent = TRUE
      )
      if (inherits(resp, "try-error")) {
        warning(resp[1])
        return(NULL)
      } else return(resp)
    },
    
    #' @description
    #' Interface method to be implemented by sub-class.
    #' @param symbol Symbol to identify the instrument.
    #' @return A list with the calling arguments.
    getInstrument = function(symbol) { 
      stopifnot(is.String(symbol))
      msg_verbose(paste0("Instrument: ", symbol, " (source: ", class(self$driver)[1], ")."))
      return(list(symbol = symbol))
    },
    
    #' @description
    #' Interface method to be implemented by sub-class.
    #' @param symbol Symbol to identify the instrument.
    #' @param range Optional period range.
    #' @param from Optional start date of period.
    #' @param to Optional end date of period.
    #' @param interval 
    #' @return A list with the calling arguments.
    getChart = function(symbol, range = NULL, from = NULL, to = NULL, interval = NULL) { 
      stopifnot(is.String(symbol))
      msg_verbose(paste0("Downloading: ", symbol, " (source: ", class(self$driver)[1], ")."))
      return(list(symbol = symbol, range = range, from = from, to = to, interval = interval))
    },
    
    #' @description
    #' Interface method to be implemented by sub-class.
    #' @param symbol Symbol to identify the instrument.
    #' @return A list with the calling arguments.
    getCashFlow = function(symbol) { 
      stopifnot(is.String(symbol))
      msg_verbose(paste0("Downloading cash flow data: ", symbol, " (source: ", class(self$driver)[1], ")."))
      return(list(symbol = symbol))
    }
  )
)



#' #' @title Execute API request with JSON
#' #' @description 
#' #' Generic helper method to execute JSON requests.
#' #' @docType methods
#' #' @import httr
#' #' @import jsonlite
#' #' @param obj An object of class `TDIConnection`.
#' #' @param url Final URL for the endpoint.
#' #' @param query URL query arguments.
#' setGeneric("request.JSON", def = function(obj, url, query) standardGeneric("request.JSON"))
#' #' @rdname request.JSON
#' setMethod("request.JSON", "TDIConnection", function(obj, url, query = NULL) {
#'   stopifnot(any(is.null(query), is.list(query)))
#'   
#'   # Call the URL and check the results.
#'   resp <- httr::GET(url, query = query)
#'   if (httr::status_code(resp) != 200) {
#'     warning(paste(url,
#'       paste0("API request failed with HTTP status: ", httr::status_code(resp)),
#'       sep = "\n"
#'     ))
#'   } else if (httr::http_type(resp) != "application/json") {
#'     stop("API did not return json", call. = FALSE)
#'   }
#'   
#'   # Parse the JSON results and check the results.
#'   resp <- try(jsonlite::fromJSON(
#'     httr::content(resp, "text", encoding = "UTF-8")), 
#'     silent = TRUE
#'   )
#'   if (inherits(resp, "try-error")) {
#'     warning(resp[1])
#'     return(NULL)
#'   } else return(resp)
#' })
