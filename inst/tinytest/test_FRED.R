# Test FRED API driver constructor.
expect_true(inherits(driver("fred"), "TDIDriver"))
expect_true(inherits(driver("fred"), "fred"))

# Test FRED connection constructor.
drv <- driver("fred")
expect_warning(expect_true(inherits(drv$connect(), "fredAPI")))
expect_warning(expect_true(inherits(drv$connect(), "TDIConnection")))
expect_warning(drv$connect(), "Federal Reserve Bank of St. Louis")

# Test FRED connection object.
con <- drv$connect()
expect_true(inherits(con$driver, "fred"))
expect_true(grepl("stlouisfed", con$conn_args$baseURL))
expect_true(hasName(con$conn_args, "api_key"))
expect_true(hasName(con$endpoints, "chart"))

# Test getChart method.
expect_warning(con$getChart("GDP"), "stlouisfed")
