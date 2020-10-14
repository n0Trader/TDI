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
