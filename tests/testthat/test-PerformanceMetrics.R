library(PerformanceAnalytics)

test_that("Return.portfolio works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  
  res <- Return.portfolio(R, weights=w)
  expect_true(is.xts(res))
  
  res2 <- Return.portfolio(R, weights=w, wealth.index=TRUE, contribution=TRUE)
  expect_true(is.xts(res2))
})

test_that("Return.annualized works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res <- Return.annualized(R)
  expect_true(is.matrix(res) || is.data.frame(res) || is.numeric(res))
})

test_that("Return.excess works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res <- Return.excess(R, Rf=0.03/12)
  expect_true(is.xts(res))
})

test_that("InformationRatio works", {
  skip_on_cran()
  data(managers)
  R_mgr <- managers[1:50, 1:3]
  R_bench <- managers[1:50, 8, drop=FALSE]
  
  res <- InformationRatio(R_mgr, R_bench)
  expect_true(is.numeric(res) || is.matrix(res))
})

test_that("TrackingError works", {
  skip_on_cran()
  data(managers)
  R_mgr <- managers[1:50, 1:3]
  R_bench <- managers[1:50, 8, drop=FALSE]
  
  res <- TrackingError(R_mgr, R_bench)
  expect_true(is.numeric(res) || is.matrix(res))
})

test_that("StdDev.annualized works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res <- StdDev.annualized(R)
  expect_true(is.numeric(res) || is.matrix(res))
})

