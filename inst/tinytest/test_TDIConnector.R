source("helper-Mock.R")

# Test the TDIConnector with Mock driver.
expect_true(R6::is.R6(TDIConnector))
expect_error(TDIConnector$connect(""), "source")
expect_true(inherits(TDIConnector$connect("MockDriver"), "MockAPI"))

# Test mocked API connection
mock <- TDIConnector$connect("MockDriver")
expect_true(is.TDIDriver(mock$driver))
expect_true(length(mock$conn_args) == 1)
expect_true(length(mock$endpoints) == 1)

# Test private method to validate query parameters
expect_equal(mock$.__enclos_env__$private$validateParam(v = 1), 1)
expect_equal(mock$.__enclos_env__$private$validateParam(v = 2), 2)
expect_equal(mock$.__enclos_env__$private$validateParam(v = 3), 1)
expect_equal(mock$.__enclos_env__$private$validateParam(x = 3), 3)

