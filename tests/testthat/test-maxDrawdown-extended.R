library(PerformanceAnalytics)

test_that("Additional drawdown methods work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1]
  
  expect_true(is.numeric(AverageDrawdown(R)))
  expect_true(is.numeric(AverageLength(R)))
  expect_true(is.numeric(AverageRecovery(R)))
  expect_true(is.numeric(DrawdownDeviation(R)))
})
