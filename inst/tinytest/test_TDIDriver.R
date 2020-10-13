source("helper-Mock.R")

# Test TDIDriver constructor.
expect_error(TDIDriver$new(), "connect")
expect_true(inherits(driver("MockDriver"), "TDIDriver"))
expect_true(inherits(driver("MockDriver"), "MockDriver"))
expect_error(driver("blablabla"), "not found")