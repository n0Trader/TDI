# Inform package user when package is installed.
.onAttach <- function( libname , pkgname ){
  packageStartupMessage("Remember to set your API variables in the environment profile.")
  packageStartupMessage("For API access run the R script startup.R (as job in RStudio).")
  packageStartupMessage("Be aware that API connections are pre-loaded in memory.")
}

# Inform package user when package is installed.
.onLoad <- function( libname , pkgname ){
  options(n0trader.verbose = TRUE)
}

