# Test Instrument base class.
testInstrument <- R6::R6Class("testInstrument", inherit = TDIInstrument,
  cloneable = FALSE, class = TRUE, # enabled S3 classes
  
  # Implementation with object initialization.
  public = list(
    initialize = function() {
      invisible(self)
    }
  )
)

ins <- testInstrument$new()
expect_true(inherits(ins, "TDIResult"))
expect_true(inherits(ins, is.Instrument()))

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
