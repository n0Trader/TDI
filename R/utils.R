#' Union class(es) for optional S4 slots.
#' @noRd
setClassUnion("_Char", c("character", "NULL"))

#' Helper to write messages in verbose mode.
#' @noRd
msg_verbose <- function(txt) {
  if (getOption("n0trader.verbose")) message(txt)
}

#' Helpers to convert Unix date format.
#' @noRd
convertUnix2Date <- function(x = NULL) {
  if (is.null(x)) { return(NULL)
  } else { as.Date(as.POSIXct(x, origin="1970-01-01")) }
}

#' Helpers to convert Unix date format.
#' @noRd
convertDate2Unix <- function(d, format="%Y-%m-%d") {
  # if (is.numeric(d)) d <- as.Date(d, origin="1970-01-01")
  if (is.null(d)) { return(NULL)
  } else { as.numeric(as.POSIXct(d, format)) }
}

#' Helpers to set today as default date.
#' @noRd
defaultToday <- function(d) {
  if (is.null(d)) return(Sys.Date())
  return(d)
}

#' Helper to check for valid string.
#' @noRd
is.String <- function(x) {
  return(all(is.character(x), nchar(x) > 0))
}

#' @title Helper to match string(s).
#' @description
#' Helper function to return a vector with string(s) matching the pattern(s).
#' The function aims to match each pattern individually (in a loop),
#' and returns an error if one of them has no match.
#' @details
#' The function first attempts an exact match.
#' When there are no exact matches it attempts to match a regular expression.
#' The regular expression aims to match at the end of the input `x`.
#'
#' In case of multiple matches on the first result is returned.
#' Depending on the `value` a value or index is returned.
#'
#' @param pattern String or vector with pattern(s) to match.
#' @param x String or vector to search.
#' @param value Boolean to retrieve the value.
#' @return Vector with column numbers or names.
#' @examples
#' match.String(c("A", "b"), c("A", "B", "C")) returns [1] 1 2
match.String <- function(pattern = null, x, value = FALSE) {
  l <- sapply(c(pattern), function(chars) {
    res <- which(x %in% chars)
    if (length(res) > 0) {
      return(if (value) x[res] else res)
    }

    # Regex match at the end of the string.
    res <- grep(paste0(chars, "$"), x, value = value, ignore.case = TRUE)
    if (length(res) > 0) {
      return(res[1])
    } else {
      stop(paste0("String not found: ", chars), call. = TRUE)
    }

  }, USE.NAMES = FALSE)
  return(c(l))
}

#' Helper to validate input arguments.
#' @noRd
validateArgs <- function(func, params) {
  stopifnot(is.function(func), is.list(params))

  # Get required arguments for the function.
  req_args <- formals(func)
  req_args["..."] <- NULL # removed dots
  req_args <- req_args[vapply(req_args, is.symbol, FUN.VALUE = TRUE)]

  # Check if all parameters are provided.
  if (all(names(req_args) %in% names(params))) {
    return(TRUE)
  } else {
    missing <- names(req_args)[!(names(req_args) %in% names(params))]
    stop("missing parameters: ", paste(missing, collapse = ", "), ".")
  }
}
