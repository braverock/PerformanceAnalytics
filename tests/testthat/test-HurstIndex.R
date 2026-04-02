library(PerformanceAnalytics)

test_that("HurstIndex correctly implements the rescaled range method", {
  skip_on_cran()

  # 1. Deterministic white noise
  set.seed(42)
  R_white_noise <- rnorm(100)

  # Manual calculation
  n <- length(R_white_noise)
  Y <- R_white_noise - mean(R_white_noise)
  Z <- cumsum(Y)
  Range <- max(Z) - min(Z)
  S <- sd(R_white_noise)
  expected_H <- log(Range / S) / log(n)

  # Function output
  h_out <- HurstIndex(R_white_noise)

  expect_equal(as.numeric(h_out), expected_H, tolerance = 1e-7)
  # Verify it is roughly around 0.5 for white noise (within reasonable bounds for n=100)
  expect_true(as.numeric(h_out) > 0.4 && as.numeric(h_out) < 0.7)

  # 2. Trending series (persistent, H > 0.5)
  # An AR(1) process with positive autocorrelation is persistent
  set.seed(42)
  R_trend <- arima.sim(list(ar = 0.9), n = 100)

  h_trend <- HurstIndex(R_trend)
  # A strongly persistent series usually exhibits a Hurst index > 0.7
  expect_true(as.numeric(h_trend) > 0.6)

  # 3. Handle multicolumn inputs correctly
  R_mat <- zoo::zoo(cbind(R_white_noise, R_trend), 1:100)
  h_mat <- HurstIndex(R_mat)

  expect_equal(dim(h_mat), c(1, 2))
  expect_equal(as.numeric(h_mat[1]), expected_H, tolerance = 1e-7)
  expect_equal(as.numeric(h_mat[2]), as.numeric(h_trend), tolerance = 1e-7)
})
