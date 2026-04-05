library(PerformanceAnalytics)
test_that("Internal utility functions in zzz.R are covered", {
  skip_on_cran()
  # Test even/odd helpers
  expect_true(PerformanceAnalytics:::even(2))
  expect_false(PerformanceAnalytics:::even(3))
  expect_true(PerformanceAnalytics:::odd(3))
  expect_false(PerformanceAnalytics:::odd(2))
  
  # Test the sd.xts alias mapping
  expect_equal(PerformanceAnalytics:::sd.xts(c(1,2,3)), sd(c(1,2,3)))
})
