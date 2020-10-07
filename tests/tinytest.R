# Automatically run tinytest for the package.
library(TDI)
if (requireNamespace("tinytest", quietly=TRUE)) {
  tinytest::test_package("TDI")
}

