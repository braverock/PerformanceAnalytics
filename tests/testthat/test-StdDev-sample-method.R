library(PerformanceAnalytics)
library(xts)

test_that("StdDev respects sample_method='ML' for maximum likelihood (fixes #3)", {
  skip_on_cran()

  # 1. Simple numeric vector
  vec <- c(0.1, 0.2, -0.05, 0.05, 0.02)
  # Standard (n-1) calculation
  res_unbiased <- as.numeric(StdDev(vec))
  expect_equal(res_unbiased, sd(vec))

  # ML (n) calculation
  res_ml <- as.numeric(StdDev(vec, sample_method = "ML"))
  expected_ml <- sd(vec) * sqrt(4 / 5)
  expect_equal(res_ml, expected_ml)

  # 2. Multivariate xts matrix with NAs (tests that NA sequences don't corrupt the length correction)
  mat <- matrix(c(vec, 0.1, NA, -0.05, 0.05, 0.02), ncol = 2)
  xts_mat <- xts(mat, order.by = as.Date("2020-01-01") + 1:5)

  res_xts_unbiased <- as.numeric(StdDev(xts_mat))
  res_xts_ml <- as.numeric(StdDev(xts_mat, sample_method = "ML"))

  expect_equal(res_xts_ml[1], res_xts_unbiased[1] * sqrt(4 / 5))
  expect_equal(res_xts_ml[2], res_xts_unbiased[2] * sqrt(3 / 4))

  # 3. Component portfolio evaluation scales the internal cov() matrix calculation
  w <- c(0.5, 0.5)
  res_comp_unbiased <- as.numeric(StdDev(xts_mat, portfolio_method = "component", weights = w, use = "pairwise.complete.obs")$StdDev)

  # When cov() is scaled by (n-1)/n, the resulting component volatility scales by sqrt((n-1)/n)
  res_comp_ml <- as.numeric(StdDev(xts_mat, portfolio_method = "component", weights = w, sample_method = "ML", use = "pairwise.complete.obs")$StdDev)

  expect_true(res_comp_ml < res_comp_unbiased) # Exact scale is non-linear for partial NAs via cov(), but MUST strictly shrink

  # 4. StdDev.annualized scales cleanly
  res_ann_unbiased <- as.numeric(StdDev.annualized(xts_mat, scale = 12))
  res_ann_ml <- as.numeric(StdDev.annualized(xts_mat, scale = 12, sample_method = "ML"))

  expect_equal(res_ann_ml[1], res_ann_unbiased[1] * sqrt(4 / 5))
  expect_equal(res_ann_ml[2], res_ann_unbiased[2] * sqrt(3 / 4))
})
