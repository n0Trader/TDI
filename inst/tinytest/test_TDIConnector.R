source("helper-Mock.R")

# Test the TDIConnector with Mock driver.
expect_true(inherits(TDIConnector, "TDIConnector"))
expect_error(TDIConnector$connect(""), "source")
expect_true(inherits(TDIConnector$connect("mock"), "MockAPI"))
expect_true(isS4(TDIConnector$connect("mock")@.drv))
expect_true(length(TDIConnector$connect("mock")@.conn_args) == 1)
expect_true(length(TDIConnector$connect("mock")@.endpoints) == 1)