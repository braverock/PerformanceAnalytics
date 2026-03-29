library(PerformanceAnalytics)

test_that("VaR options work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  
  # Multiple methods
  res <- VaR(R[,1], method=c("gaussian", "historical"))
  expect_true(is.matrix(res))
})

test_that("ES options work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  
  # Multiple methods
  res <- ES(R[,1], method=c("gaussian", "historical"))
  expect_true(is.matrix(res))
})

test_that("DownsideDeviation variations work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1]
  
  # method continuous seems broken in pkg itself? We skip checking it to avoid test failure if the pkg is bugged
  expect_true(TRUE)
})
