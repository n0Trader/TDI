# Test union class(es) for S4 slots.
expect_true(isClass("_Char"))

# Test converting dates to Unix format.
expect_error(convertDate2Unix(""), "format")
expect_equal(convertDate2Unix(NULL), NULL)
expect_equal(convertDate2Unix("1970-01-01"), 0)
expect_equal(convertDate2Unix("2020-01-01"), 1577836800)
expect_true(convertDate2Unix(Sys.Date()) > 1577836800)

# Test converting Unix dates.
expect_error(convertUnix2Date(""), "character string")
expect_equal(convertUnix2Date(NULL), NULL)
expect_equal(convertUnix2Date(0), as.Date("1970-01-01"))
expect_equal(convertUnix2Date(1577836800), as.Date("2020-01-01"))

# Test string validation.
expect_true(is.String("ABC"))
expect_false(is.String(""))
expect_false(is.String(NA))
expect_false(is.String(NULL))
