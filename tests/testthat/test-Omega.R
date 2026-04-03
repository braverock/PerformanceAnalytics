library(PerformanceAnalytics)

test_that("Omega(method='interp') relies on true trapezoidal integration matching method='simple' (fixes #65)", {
  skip_on_cran()
  skip_if_not_installed("Hmisc")
  
  set.seed(42)
  # Large random sample to minimize empirical steps
  x <- rnorm(5000, mean=0.01, sd=0.05)
  L <- 0.0
  
  # 1. Simple method computes empirical ratio (expected value) exactly
  res_simple <- Omega(x, L=L, method="simple", output="point")
  
  # 2. Interpolation method computes area using trapezoidal integration
  res_interp <- Omega(x, L=L, method="interp", output="point")
  
  # The old broken implementation used simple cumsum steps without dx weights, yielding ratios > 3.0 instead of ~ 1.45!
  # By substituting true trapezoidal integration, the two methods theoretically converge within a rounding tolerance
  expect_true(as.numeric(res_interp) > 1.3 && as.numeric(res_interp) < 1.6)
  expect_equal(as.numeric(res_simple), as.numeric(res_interp), tolerance = 1e-1)
  
  # Check full output renders structurally correctly
  res_full <- Omega(x, L=L, method="interp", output="full")
  expect_true(is.matrix(res_full))
  expect_equal(nrow(res_full), length(unique(x)) + 1)
})

test_that("Omega gracefully evaluates strict mathematical boundary conditions", {
  skip_on_cran()
  skip_if_not_installed("Hmisc")
  
  # 1. No Downside Risk (All returns strictly > L)
  R_high <- c(0.05, 0.10, 0.15, 0.20)
  # Denominator evaluates to 0, so Omega should be Inf
  expect_equal(as.numeric(Omega(R_high, L=0, method="simple")), Inf)
  # The empirical CDF starts at 0.05, so the area from 0 to 0.05 is 0. 
  # The trapz integral might struggle if the CDF domain doesn't reach L, so we bounded it.
  expect_error(Omega(R_high, L=0, method="interp"), NA)
  
  # 2. No Upside Potential (All returns strictly < L)
  R_low <- c(-0.20, -0.15, -0.10, -0.05)
  # Numerator evaluates to 0, so Omega should be 0
  expect_equal(as.numeric(Omega(R_low, L=0, method="simple")), 0)
  expect_error(Omega(R_low, L=0, method="interp"), NA)
  
  # 3. Missing values (NAs) are gracefully omitted
  R_na <- c(-0.10, 0.05, NA, 0.15, NA)
  expect_error(Omega(R_na, L=0, method="simple"), NA)
  expect_error(Omega(R_na, L=0, method="interp"), NA)
  
  # 4. Extreme Thresholds (L deeply outside the data domain)
  R_norm <- c(-0.10, 0.00, 0.10)
  # Should not crash on approxfun out-of-bounds interpolation
  expect_error(Omega(R_norm, L=100, method="interp"), NA)
  expect_error(Omega(R_norm, L=-100, method="interp"), NA)
})
