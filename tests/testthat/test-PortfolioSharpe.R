library(PerformanceAnalytics)
library(xts)

test_that("Portfolio Sharpe ratio explicitly incorporates Rf in numerator and denominator (fixes #100, #142)", {
  skip_on_cran()

  dates <- as.Date("2020-01-01") + 0:4
  R <- xts(matrix(c(
    0.10, -0.05, 0.05, -0.10, 0.02,
    -0.05, 0.05, 0.10, -0.10, 0.05
  ), ncol = 2), order.by = dates)
  colnames(R) <- c("A", "B")

  w <- c(0.5, 0.5)

  # A. Scalar Rf should mathematically match raw return standard deviation behavior due to shift-invariance
  Rf_scalar <- 0.02
  res_scalar <- SharpeRatio(R, Rf = Rf_scalar, weights = w, FUN = "StdDev")

  man_num_scalar <- mean(R %*% w) - Rf_scalar
  man_den_scalar <- sd(R %*% w) # Scalar shift invariance
  expect_equal(as.numeric(res_scalar), man_num_scalar / man_den_scalar, tolerance = 1e-8)

  # B. Vector Rf should correctly subtract period-by-period before evaluating BOTH mean and standard deviation
  Rf_vec <- xts(c(0.01, 0.02, 0.03, 0.01, 0.02), order.by = dates)
  res_vec <- SharpeRatio(R, Rf = Rf_vec, weights = w, FUN = "StdDev")

  xR <- Return.excess(R, Rf_vec)
  man_num_vec <- mean(xR %*% w)
  man_den_vec <- sd(xR %*% w)
  expect_equal(as.numeric(res_vec), man_num_vec / man_den_vec, tolerance = 1e-8)

  # C. Degenerate case where excess returns strictly average to 0.0 but have variance
  Rp <- R %*% w
  Rf_zero_mean <- xts(matrix(mean(Rp), 5, 1), order.by = dates)
  res_match <- SharpeRatio(R, Rf = Rf_zero_mean, weights = w, FUN = "StdDev")
  expect_equal(as.numeric(res_match), 0)
})
