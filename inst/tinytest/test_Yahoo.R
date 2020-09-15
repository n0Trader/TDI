# Test Yahoo Finance API driver constructor.
expect_true(inherits(driver("yahoo"), "TDIDriver"))
expect_true(inherits(driver("yahoo"), "yahoo"))

# Test Yahoo connection constructor.
expect_true(inherits(apiConnect(driver("yahoo")), "YahooAPI"))
expect_true(inherits(apiConnect(driver("yahoo")), "TDIConnection"))

# Validate methods
expect_true(hasMethod("getSymbol", "YahooAPI"))

# Test Yahoo connection object.
con <- apiConnect(driver("yahoo"))
expect_true(inherits(con@.drv, "yahoo"))
expect_true(inherits(con@.handle, "curl_handle"))
expect_true(grepl("finance.yahoo.com", con@.conn_args$baseURL))
expect_true(nchar(con@.endpoints$quotes) > 0)
