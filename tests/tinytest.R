# Automatically run tinytest for the package.
if (requireNamespace("tinytest", quietly=TRUE)) {
  tinytest::test_package("TDI")
}

