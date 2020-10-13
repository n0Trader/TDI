# Test Instrument constructor.
expect_true(inherits(Instrument$new(symbol = "ABC", source = "XYZ"), is.Instrument()))
ins <- Instrument$new(symbol = "ABC", source = "XYZ", currency = "EUR", type = "TYP")
expect_true(inherits(ins, "TDIResult"))
expect_true(inherits(ins, is.Instrument()))
expect_equal(ins$symbol, "ABC")
expect_equal(ins$sources, list("XYZ"))
expect_equal(ins$currency, "EUR")
expect_equal(ins$type, "TYP")

# Test loading historical prices and read results.
prices <- readRDS("./data/stock.rda")
ins$setSeries(prices)
expect_equal(nrow(ins$series), nrow(prices))
expect_equal(nrow(ins$getReturn()), nrow(prices))
expect_equal(nrow(ins$getWealthIndex()), nrow(prices))

# Test method getSession
last <- zoo::index(xts::last(prices))
expect_equal(zoo::index(ins$getSession(last)), last)
expect_equal(zoo::index(ins$getSession(last, 0)), last)
expect_equal(ins$getSession(last, 1), NULL)
expect_equal(ins$getSession("1970-01-01"), NULL)
