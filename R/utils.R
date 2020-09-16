# Helpers to convert date formats.
convertUnix2Date <- function(x) {
  as.Date(as.POSIXct(x, origin="1970-01-01"))
}
convertDate2Unix <- function(d, format="%Y-%m-%d") {
  if (is.numeric(d)) d <- as.Date(d, origin="1970-01-01")
  as.numeric(as.POSIXct(d, format))
}
