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
expect_true(hasMethod("getSymbol", is.Instrument()))
expect_true(hasMethod("getSeries", is.Instrument()))
expect_true(hasMethod("getSession", is.Instrument()))
expect_true(hasMethod("getReturn", is.Instrument()))
expect_true(hasMethod("getWealthIndex", is.Instrument()))
