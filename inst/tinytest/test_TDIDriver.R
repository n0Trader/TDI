source("helper-Mock.R")

# Test TDIDriver constructor.
expect_true(inherits(driver("mock"), "TDIDriver"))
expect_true(inherits(driver("mock"), "mock"))
expect_error(driver("blablabla"))