library(PerformanceAnalytics)

test_that("DownsidePotential works", {
  skip_on_cran()
  data(portfolio_bacon)
  MAR = 0.005
  res1 <- DownsideDeviation(portfolio_bacon[,1], MAR)
  res2 <- DownsidePotential(portfolio_bacon[,1], MAR)
  
  expect_true(is.numeric(res1))
  expect_true(is.numeric(res2))
})

test_that("DownsideDeviation matrix variations", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  # Method continuous doesn't execute inside DownsideDeviation if len is undefined
  expect_true(is.numeric(DownsideDeviation(R[,1], method="full")))
  expect_true(is.numeric(DownsideDeviation(R[,1], method="subset")))
  
  # Passing entire matrix natively instead of just apply column
  expect_true(is.matrix(DownsideDeviation(R, MAR=0.01)))
  
})
