library(PerformanceAnalytics)

test_that("StdDev tests", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  
  res1 <- StdDev(R, portfolio_method="component", weights=w)
  expect_true(is.list(res1))
  
  res2 <- StdDev(R, clean="boudt", portfolio_method="component", weights=w)
  expect_true(is.list(res2))
  
  res3 <- StdDev(R[,1], clean="geltner")
  expect_true(is.numeric(res3))
})

test_that("DownsideDeviation variations", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res1 <- DownsideDeviation(R, MAR=0.01)
  expect_true(is.numeric(res1))
  
  res2 <- SemiDeviation(R)
  expect_true(is.numeric(res2))
})

test_that("SharpeRatio variations", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  sr_std <- SharpeRatio(R, FUN="StdDev")
  expect_true(is.matrix(sr_std) || is.data.frame(sr_std) || is.numeric(sr_std))
  
  sr_mod <- SharpeRatio(R, FUN="VaR")
  expect_true(is.matrix(sr_mod) || is.data.frame(sr_mod) || is.numeric(sr_mod))
  
  sr_es <- SharpeRatio(R, FUN="ES")
  expect_true(is.matrix(sr_es) || is.data.frame(sr_es) || is.numeric(sr_es))
})

