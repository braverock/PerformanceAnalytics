library(PerformanceAnalytics)

test_that("CDAR.alpha and CDAR.beta cover all branches", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:2]
  Rm <- edhec[1:50, 3]

  # 1. Dimension mismatch
  expect_error(CDaR.alpha(R, Rm[1:10]), "The number of rows of the return series and the optimal portfolio should be equal")
  expect_error(CDaR.beta(R, Rm[1:10]), "The number of rows of the return series and the benchmark portfolio should be equal")

  # 2. Univariate max / average
  expect_true(is.numeric(CDaR.alpha(R[,1], Rm, type="max")))
  expect_true(is.numeric(CDaR.alpha(R[,1], Rm, type="average")))

  # 3. Multivariate loop (no weights)
  res_alpha <- CDaR.alpha(R, Rm)
  expect_equal(dim(res_alpha), c(1, 2))
  res_beta <- CDaR.beta(R, Rm)
  expect_equal(dim(res_beta), c(1, 2))

  # 4. Multivariate with weights
  w <- c(0.5, 0.5)
  res_alpha_w <- CDaR.alpha(R, Rm, weights = w)
  expect_true(is.numeric(as.numeric(res_alpha_w)))
  res_beta_w <- CDaR.beta(R, Rm, weights = w)
  expect_true(is.numeric(as.numeric(res_beta_w)))
})
