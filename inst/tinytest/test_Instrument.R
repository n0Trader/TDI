# Test Instrument constructor.
expect_true(inherits(Instrument(symbol = "ABC", source = "XYZ"), is.Instrument()))
ins <- Instrument(symbol = "ABC", source = "XYZ", currency = "EUR", type = "TYP")
expect_true(inherits(ins, "TDIResult"))
expect_true(inherits(ins, is.Instrument()))
expect_equal(ins@.symbol, "ABC")
expect_equal(ins@.sources, list("XYZ"))
expect_equal(ins@.currency, "EUR")
expect_equal(ins@.type, "TYP")

# Test the methods exist for the class.
expect_true(hasMethod("setSeries", is.Instrument()))
expect_true(hasMethod("addSerie", is.Instrument()))
expect_true(hasMethod("getSymbol", is.Instrument()))
expect_true(hasMethod("getSeries", is.Instrument()))
expect_true(hasMethod("getSession", is.Instrument()))
expect_true(hasMethod("getReturn", is.Instrument()))
expect_true(hasMethod("getWealthIndex", is.Instrument()))

# Test loading historical prices and read results.
prices <- readRDS("./data/stock.rda")
ins <- setSeries(ins, prices)
expect_equal(nrow(getSeries(ins)), nrow(prices))
expect_equal(nrow(getReturn(ins)), nrow(prices))
expect_equal(nrow(getWealthIndex(ins)), nrow(prices))

# Test method getSession
last <- zoo::index(xts::last(prices))
expect_equal(zoo::index(getSession(ins, last)), last)
expect_equal(zoo::index(getSession(ins, last, 0)), last)
expect_equal(getSession(ins, last, 1), NULL)
expect_equal(getSession(ins, "1970-01-01"), NULL)
