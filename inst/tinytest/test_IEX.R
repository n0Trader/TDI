# Test IEX Cloud API driver constructor.
expect_true(inherits(driver("iex"), "TDIDriver"))
expect_true(inherits(driver("iex"), "iex"))

# Test IEX connection constructor.
drv <- driver("iex")
expect_true(inherits(drv$connect(), "iexAPI"))
expect_true(inherits(drv$connect(), "TDIConnection"))

# Test IEX connection object.
con <- drv$connect()
expect_true(inherits(con$driver, "iex"))
expect_true(grepl("iexapis", con$conn_args$baseURL))
expect_true(hasName(con$conn_args, "api_token"))
expect_true(hasName(con$conn_args, "api_version"))
expect_true(hasName(con$endpoints, "chart"))
expect_silent(con$getChart("AAPL", range = "1d"))

# Test IEX API parameter validation.
expect_equal(con$.__enclos_env__$private$validateParam(range = "5d"), "5d")
expect_equal(con$.__enclos_env__$private$validateParam(range = "0d"), "3m")
expect_equal(con$.__enclos_env__$private$validateParam(interval = "1mo"), "1mo")
expect_equal(con$.__enclos_env__$private$validateParam(interval = "0d"), "1d")
