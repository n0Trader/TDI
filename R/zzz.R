# Inform package user when package is installed.
.onAttach <- function( libname , pkgname ){
  packageStartupMessage("Please remember to set your API variables in the environment profile.")
  packageStartupMessage("For API access run the R script startup.R (as job in RStudio).")
}

# Inform package user when package is installed.
.onLoad <- function( libname , pkgname ){
  options(n0trader.verbose = TRUE)
}

