library(PerformanceAnalytics)
library(xts)

test_that("RachevRatio evaluates correctly across standard parameter bounds", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  
  # A. Univariate execution
  res1 <- RachevRatio(R[,1])
  expect_true(is.numeric(res1))
  
  # B. Multivariate execution
  res2 <- RachevRatio(R)
  expect_equal(dim(res2), c(1, 4))
  
  # C. Portfolio component weighting
  res3 <- RachevRatio(R, weights = w)
  expect_true(is.numeric(res3))
  
  # D. RPESE Standard Errors
  if (requireNamespace("RPESE", quietly = TRUE)) {
    res4 <- RachevRatio(R, SE = TRUE)
    expect_equal(dim(res4), c(3, 4)) # Ratio + IFiid + IFcor
  }
})
