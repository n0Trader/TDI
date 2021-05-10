#' @title Check for TA Indicator class
#' @description
#' Helper to check the object class.
#' If the input `x` is empty it returns the class name.
#' @param x Object to check.
#' @return Boolean result or alternative the class name.
#' @export
is.TAIndicator <- function(x = NULL) {
  class = "TAIndicator"
  if (is.null(x)) { return(class)
  } else return(inherits(x, class))
}

#' @title Technical analysis indicator factory
#' @description
#' Constructor to instantiate object(s) of class `TAIndicator`.
#' It calls the object class constructor with provided parameters.
#' @param label Label for the indicator.
#' @param method Method to calculate the indicator.
#' @param columns Input data column(s).
#' @param args Input parameters for the indicator.
#' @return An object of class `TAIndicator`.
#' @examples
#' # Call the factory with technical indicator setup.
#' ta <- taFactory(label = "ADX", method = "TTR::ADX",
#'   columns = list(HLC = c("High", "Low", "Close")),
#'   args = list(n = 10, maType = "EMA")
#' )
#' @export
taFactory <- function(label, method, columns, args = NULL) {
  # Call the indicator constructor.
  params <- c(label = label, method = method)
  if (is.list(columns)) params <- c(params, columns = list(columns))
  if (is.list(args)) params <- c(params, args = list(args))
  do.call(taIndicator$new, params)
}

#' @title Technical analysis indicator (R6 class constructor)
#' @description
#' An R6 class for technical analysis indicator(s).
#' Its purpose is to facilitate calculations on time-series of data.
#' The fields specify the `func` and parameters for the calculation(s).
#' The indicator(s) can be constructed with the function `taFactory()`.
#'
#' @import R6
#' @export
TAIndicator <- R6::R6Class(is.TAIndicator(), inherit = baseTDI,
  class = TRUE, # enable serialize and S3 methods!!
  cloneable = FALSE,

  public = list(
    #' @field label Unique label for the indicator.
    label = NULL,
    #' @field method Indicator calculation method.
    method = as.character(),
    #' @field columns List with input data column(s).
    columns = list(),
    #' @field args Arguments for the calculation.
    args = list(),

    #' @description
    #' Object constructor for class `taIndicator`.
    #' @param label Label for the indicator.
    #' @param method Method to calculate the indicator.
    #' @param columns List of input data column(s).
    #' @param args List of arguments for the indicator function.
    #' @return An object of class `taIndicator`.
    initialize = function(label, method, columns, args = list()) {
      stopifnot(
        is.String(label),
        is.String(method),
        is.list(columns),
        is.list(args)
      )

      # Try to locate the calculation function.
      func <- try(eval(parse(text = method)))
      stopifnot(is.function(func))
      stopifnot(validateArgs(func, c(columns, args)))

      # Initialize the object.
      self$label <- label
      self$method <- method
      self$columns <- columns
      self$args <- args
      private$.Method <- func
      invisible(self)
    },

    #' @description
    #' Calculate the indicator for the time series.
    #' @param x Time-series object with input data.
    #' @return xts Time-serie with the indicator(s).
    calculate = function(x) {
      stopifnot(inherits(x, "xts"))

      # Collect input data for the calculation.
      data <- list()
      for (i in 1:length(self$columns)) {
        arg_name <- names(self$columns)[i]
        if (is.null(arg_name)) {
          data[[i]] <- columns.xts(x, self$columns[[i]])
        } else {
          data[[arg_name]] <- columns.xts(x, self$columns[[i]])
        }
      }

      # Execute the calculation.
      params <- c(data, self$args)
      return(do.call(private$.Method, params))
    }
  ),

  # Private for self.
  private = list(
    .Method = NULL # indicator calculation method.
  )
)
