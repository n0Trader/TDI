# Test Yahoo Finance API driver constructor.
expect_true(inherits(driver("yahoo"), "TDIDriver"))
expect_true(inherits(driver("yahoo"), "yahoo"))

# Test Yahoo connection constructor.
drv <- driver("yahoo")
expect_true(inherits(drv$connect(), "yahooAPI"))
expect_true(inherits(drv$connect(), "TDIConnection"))

# Test Yahoo connection object.
con <- drv$connect()
expect_true(inherits(con$driver, "yahoo"))
expect_true(grepl("finance.yahoo.com", con$conn_args$baseURL))
expect_true(hasName(con$endpoints, "chart"))

# Test API calls.
expect_silent(con$getChart("AAPL"))
expect_silent(con$getInstrument("MSFT")$isValid())
expect_silent(con$getCashFlow("TSLA")$isValid())

# Test Yahoo API parameter validation.
expect_equal(con$.__enclos_env__$private$validateParam(range = "1d"), "1d")
expect_equal(con$.__enclos_env__$private$validateParam(range = "0d"), "1d")
expect_equal(con$.__enclos_env__$private$validateParam(range = "5y"), "5y")
expect_equal(con$.__enclos_env__$private$validateParam(interval = "1mo"), "1mo")
expect_equal(con$.__enclos_env__$private$validateParam(interval = "0d"), "1d")

