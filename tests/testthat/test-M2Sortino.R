library(PerformanceAnalytics)
library(xts)

test_that("M2Sortino processes accurately", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:3]
  Rb <- edhec[1:50, 4]
  
  # A. Univariate
  res1 <- M2Sortino(R[,1], Rb)
  expect_true(is.numeric(res1))
  
  # B. Multivariate
  res2 <- M2Sortino(R, Rb)
  expect_equal(dim(res2), c(1, 3))
  
  # C. Different MAR
  res3 <- M2Sortino(R, Rb, MAR = 0.05)
  expect_true(all(res2 != res3))
})
