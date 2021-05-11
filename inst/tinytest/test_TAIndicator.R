# Test the TAIndicator class
expect_true(R6::is.R6Class(TAIndicator))
expect_error(TAIndicator$new(), "label")
expect_error(TAIndicator$new(label = "L"), "method")
expect_error(TAIndicator$new(label = "L", method = "m"), "columns")
expect_error(TAIndicator$new(label = "L", method = "m", columns = list()), "function")
expect_error(TAIndicator$new(label = "L", method = "mean", columns = list()), "missing parameters")

ind <- TAIndicator$new(
  label = "L", method = "mean", columns = list("x" = "A")
)
expect_true(inherits(ind, is.TAIndicator()))

# Test the factory function with one 'columns' element input
json <- '{"label": "SMA", "method": "TTR::SMA", "columns": [{"x": ["Close"]}]}'
ta <- jsonlite::fromJSON(json)
expect_silent(taFactory(ta))

# Test the factory function with one 'columns' element and 'args' input
json <- '{"label": "RSI", "method": "TTR::RSI", "columns": [{"price": ["Close"]}], "args": [{"n": 14, "maType": "EMA"}]}'
ta <- jsonlite::fromJSON(json)
expect_silent(taFactory(ta))

# Test the factory function with multiple 'columns' and 'args' input
json <- '{"label": "ATR", "method": "TTR::ATR", "columns": [{"HLC": ["High", "Low", "Close"]}], "args": [{"n": 14, "maType": "EMA"}]}'
ta <- jsonlite::fromJSON(json)
expect_silent(taFactory(ta))
