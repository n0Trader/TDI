# Test FRED API driver constructor.
expect_true(inherits(driver("fred"), "TDIDriver"))
expect_true(inherits(driver("fred"), "fred"))

# Test FRED connection constructor.
expect_warning(expect_true(inherits(apiConnect(driver("fred")), "fredAPI")))
expect_warning(expect_true(inherits(apiConnect(driver("fred")), "TDIConnection")))
expect_warning(apiConnect(driver("fred")), "Federal Reserve Bank of St. Louis")

# Validate methods
expect_true(hasMethod("getSymbol", "fredAPI"))

# Test FRED connection object.
con <- apiConnect(driver("fred"))
expect_true(inherits(con@.drv, "fred"))
expect_true(grepl("fred", con@.conn_args$baseURL))
expect_true(is.vector(con@.conn_args$api_key))
expect_true(is.vector(con@.endpoints$series))
