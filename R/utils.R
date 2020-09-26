# Union class(es) for optional S4 slots.
setClassUnion("_Char", c("character", "NULL"))

# Helper to write messages in verbose mode.
msg <- function(txt) {
  if (getOption("verbose")) message(txt)
}

# Helpers to convert date formats.
convertUnix2Date <- function(x = NULL) {
  if (is.null(x)) { return(NULL) 
  } else { as.Date(as.POSIXct(x, origin="1970-01-01")) }
}
convertDate2Unix <- function(d, format="%Y-%m-%d") {
  if (is.numeric(d)) d <- as.Date(d, origin="1970-01-01")
  if (is.null(d)) { return(NULL)
  } else { as.numeric(as.POSIXct(d, format)) }
}

# Helper to check for valid string.
is.String <- function(x) {
  return(all(is.character(x), nchar(x) > 0))
}
