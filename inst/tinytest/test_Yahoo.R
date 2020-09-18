# Test Yahoo Finance API driver constructor.
expect_true(inherits(driver("yahoo"), "TDIDriver"))
expect_true(inherits(driver("yahoo"), "yahoo"))

# Test Yahoo connection constructor.
expect_true(inherits(apiConnect(driver("yahoo")), "YahooAPI"))
expect_true(inherits(apiConnect(driver("yahoo")), "TDIConnection"))

# Validate methods
expect_true(hasMethod("getSeries", "YahooAPI"))

# Test Yahoo connection object.
con <- apiConnect(driver("yahoo"))
expect_true(inherits(con@.drv, "yahoo"))
expect_true(grepl("finance.yahoo.com", con@.conn_args$baseURL))
expect_true(is.vector(con@.endpoints$series))

# Test parameter range validation
expect_equal(validRange(con, "1d"), "1d")
expect_equal(validRange(con, "0d"), "5y")
expect_equal(validRange(con), "5y")

# Test parameter interval validation
expect_equal(validInterval(con, "1mo"), "1mo")
expect_equal(validInterval(con, "0d"), "1d")
expect_equal(validInterval(con), "1d")
