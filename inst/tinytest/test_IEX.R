# Test IEX Cloud API driver constructor.
expect_true(inherits(driver("iex"), "TDIDriver"))
expect_true(inherits(driver("iex"), "iex"))

# Test IEX connection constructor.
expect_true(inherits(apiConnect(driver("iex")), "TDIConnection"))
expect_true(inherits(apiConnect(driver("iex")), "iexAPI"))

# Validate methods
expect_true(hasMethod("getSymbol", "iexAPI"))

# Test IEX connection object.
con <- apiConnect(driver("iex"))
expect_true(inherits(con@.drv, "iex"))
expect_true(grepl("iexapis", con@.conn_args$baseURL))
expect_true(is.vector(con@.conn_args$api_token))
expect_true(is.vector(con@.endpoints$series))

# Test parameter range validation
expect_equal(validRange(con, "5d"), "5d")
expect_equal(validRange(con, "0d"), "3m")
expect_true(is.null(validRange(con)))

# Test parameter interval validation
expect_equal(validInterval(con, "1mo"), "1mo")
expect_equal(validInterval(con, "0d"), "1d")
expect_equal(validInterval(con), "1d")
