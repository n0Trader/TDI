source("helper-Mock.R")

# Test the TDIConnector with Mock driver.
expect_true(R6::is.R6(TDIConnector))
expect_error(TDIConnector$connect(""), "source")
expect_true(inherits(TDIConnector$connect("MockDriver"), "MockAPI"))

mock <- TDIConnector$connect("MockDriver")
expect_true(is.TDIDriver(mock$driver))
expect_true(length(mock$conn_args) == 1)
expect_true(length(mock$endpoints) == 1)