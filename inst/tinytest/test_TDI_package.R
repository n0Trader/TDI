# Check installed packages.
expect_true("xts" %in% rownames(installed.packages()))
expect_true("zoo" %in% rownames(installed.packages()))
expect_true("TDI" %in% rownames(installed.packages()))

