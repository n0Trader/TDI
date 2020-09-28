# Inform package user when package is installed.
.onAttach <- function( libname , pkgname ){
  packageStartupMessage("Please remember to set your API variables in the environment profile.")
}