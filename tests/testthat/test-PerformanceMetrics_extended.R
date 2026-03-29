library(PerformanceAnalytics)

test_that("Return.convert works", {
  skip_on_cran()
  data(edhec)
  # positive returns to avoid NaN in log
  R <- abs(edhec[1:50, 1:4])
  xtsAttributes(R) <- list(coredata_content = "discreteReturn")
  
  if (exists("Return.convert", asNamespace("PerformanceAnalytics"))) {
    res1 <- PerformanceAnalytics:::Return.convert(R, destinationType="log")
    expect_true(is.xts(res1))
    
    res2 <- PerformanceAnalytics:::Return.convert(res1, destinationType="discrete")
    expect_true(is.xts(res2))
  }
})

test_that("mean.arithmetic works", {
  skip_on_cran()
  data(edhec)
  
  res <- PerformanceAnalytics:::mean.arithmetic(edhec[,1])
  expect_true(is.numeric(res))
})

test_that("mean.geometric works", {
  skip_on_cran()
  data(edhec)
  
  res <- PerformanceAnalytics:::mean.geometric(edhec[,1])
  expect_true(is.numeric(res))
})

test_that("ES.CornishFisher.portfolio works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  p <- 0.95
  
  if (exists("ES.CornishFisher.portfolio", asNamespace("PerformanceAnalytics"))) {
    res <- PerformanceAnalytics:::ES.CornishFisher.portfolio(p=p, w=w, mu=colMeans(R), sigma=cov(R), M3=M3.MM(R), M4=M4.MM(R))
    expect_true(is.list(res))
  }
})

test_that("NCEHelper.c works through MM.NCE options", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:3] * 100
  
  # Hit the rest of NCE code branches
  res1 <- MM.NCE(R, B = matrix(0, ncol=1, nrow=3))
  expect_true(is.list(res1))
})

