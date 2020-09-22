# OHLC test-cases, including underlying supporting functions.
# Load stock test data (originating from quantmod::getSymbols())
stock <- readRDS("./data/stock.rda")
cols <- colnames(stock)

# Test has OHLC functions to be true.
expect_true(has.Op(stock))
expect_true(has.Hi(stock))
expect_true(has.Lo(stock))
expect_true(has.Cl(stock))
expect_true(has.Vo(stock))
expect_true(has.OHLC(stock))
expect_true(length(has.OHLC(stock, all = FALSE)) == 4)

# Test which for OHLC functions.
expect_equal(has.Op(stock, which = TRUE), 1)
expect_equal(has.Hi(stock, which = TRUE), 2)
expect_equal(has.Lo(stock, which = TRUE), 3)
expect_equal(has.Cl(stock, which = TRUE), 4)
expect_equal(has.Vo(stock, which = TRUE), 5)
expect_equal(has.OHLC(stock, which = TRUE), c(1,2,3,4))

# Test return correct OHLC column(s).
expect_identical(colnames(Op(stock)), cols[1])
expect_identical(colnames(Hi(stock)), cols[2])
expect_identical(colnames(Lo(stock)), cols[3])
expect_identical(colnames(Cl(stock)), cols[4])
expect_identical(colnames(OHLC(stock)), cols[c(1,2,3,4)])

# Test "Open" columns with symbol "OPEN".
colnames(stock) <- gsub("MSFT", "OPEN", cols)
expect_true(has.Op(stock))
expect_equal(has.Op(stock, which = TRUE), 1)
expect_identical(colnames(Op(stock)), colnames(stock)[1])

# Test "High" columns with symbol "HIGH".
colnames(stock) <- gsub("MSFT", "HIGH", cols)
expect_true(has.Hi(stock))
expect_equal(has.Hi(stock, which = TRUE), 2)
expect_identical(colnames(Hi(stock)), colnames(stock)[2])

# Test "Low" columns with symbol "LOW".
colnames(stock) <- gsub("MSFT", "LOW", cols)
expect_true(has.Lo(stock))
expect_equal(has.Lo(stock, which = TRUE), 3)
expect_identical(colnames(Lo(stock)), colnames(stock)[3])

# Test "Close" columns with symbol "CLOSE".
colnames(stock) <- gsub("MSFT", "CLOSE", cols)
expect_true(has.Cl(stock))
expect_equal(has.Cl(stock, which = TRUE), 4)
expect_identical(colnames(Cl(stock)), colnames(stock)[4])

# Test "Volume" columns with symbol "VOLUME".
colnames(stock) <- gsub("MSFT", "VOLUME", cols)
expect_true(has.Vo(stock))
expect_equal(has.Vo(stock, which = TRUE), 5)
expect_identical(colnames(Vo(stock)), colnames(stock)[5])

