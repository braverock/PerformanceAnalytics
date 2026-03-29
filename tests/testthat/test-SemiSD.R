library(PerformanceAnalytics)

test_that("SemiSD works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res1 <- SemiSD(R)
  expect_true(is.numeric(res1) || is.matrix(res1))
  
  w <- rep(0.25, 4)
  res2 <- SemiSD(R, portfolio_method="component", weights=w)
  expect_true(is.matrix(res2) || is.list(res2))
})

test_that("table.Arbitrary works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res1 <- table.Arbitrary(R, metrics=c("mean", "sd"), metricsNames=c("Mean", "StdDev"))
  expect_true(is.data.frame(res1))
})

test_that("table.RollingPeriods works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res <- table.RollingPeriods(R, periods=c(12, 24, 36))
  expect_true(is.data.frame(res))
})
