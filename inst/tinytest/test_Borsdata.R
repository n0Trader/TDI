# Test Borsdata API driver constructor.
expect_true(inherits(driver("bdata"), "TDIDriver"))
expect_true(inherits(driver("bdata"), "bdata"))

# Test Borsdata connection constructor.
drv <- driver("bdata")
expect_true(inherits(drv$connect(), "bdataAPI"))
expect_true(inherits(drv$connect(), "TDIConnection"))

# Test Borsdata connection object.
con <- drv$connect()
expect_true(inherits(con$driver, "bdata"))
expect_true(grepl("borsdata.se", con$conn_args$baseURL))
expect_true(hasName(con$endpoints, "instruments"))

# Test API calls.
expect_silent(con$getInstrument("RAIL")$isValid())
# expect_silent(con$getChart("AAPL"))
# expect_silent(con$getCashFlow("TSLA")$isValid())
