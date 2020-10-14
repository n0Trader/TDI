# Testing internal functions with triple-colon (`:::`).

# Test union class(es) for S4 slots.
expect_true(isClass("_Char"))

# Test message option.
options("n0trader.verbose" = TRUE)
expect_message(TDI:::msg_verbose("HI!"))
options("n0trader.verbose" = FALSE)
expect_silent(TDI:::msg_verbose("HI!"))

# Test converting dates to Unix format.
expect_error(TDI:::convertDate2Unix(""), "format")
expect_equal(TDI:::convertDate2Unix(NULL), NULL)
expect_equal(TDI:::convertDate2Unix("1970-01-01"), 0)
expect_equal(TDI:::convertDate2Unix("2020-01-01"), 1577836800)
expect_true(TDI:::convertDate2Unix(Sys.Date()) > 1577836800)

# Test converting Unix dates.
expect_error(TDI:::convertUnix2Date(""), "character string")
expect_equal(TDI:::convertUnix2Date(NULL), NULL)
expect_equal(TDI:::convertUnix2Date(0), as.Date("1970-01-01"))
expect_equal(TDI:::convertUnix2Date(1577836800), as.Date("2020-01-01"))

# Test string validation.
expect_true(TDI:::is.String("ABC"))
expect_false(TDI:::is.String(""))
expect_false(TDI:::is.String(NA))
expect_false(TDI:::is.String(NULL))
