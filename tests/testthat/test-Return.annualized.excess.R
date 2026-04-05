library(PerformanceAnalytics)
library(xts)

test_that("Return.annualized.excess calculates expected vectors", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:3]
  Rb <- edhec[1:50, 4]
  
  # A. Univariate
  res1 <- Return.annualized.excess(R[,1], Rb)
  expect_true(is.numeric(res1))
  
  # B. Multivariate
  res2 <- Return.annualized.excess(R, Rb)
  expect_equal(dim(res2), c(1, 3))
  
  # C. Arithmetic average
  res3 <- Return.annualized.excess(R, Rb, geometric = FALSE)
  expect_true(all(res2 != res3))
  
  # D. Explicit Scale (no detection)
  res4 <- Return.annualized.excess(as.numeric(R[,1]), as.numeric(Rb), scale = 12)
  expect_true(is.numeric(res4))
})
