library(PerformanceAnalytics)

test_that("CAPM.beta handles numeric vectors correctly", {
  skip_on_cran()
  
  set.seed(42)
  Ra <- rnorm(50, mean=0.02)
  Rb <- rnorm(50, mean=0.01)
  
  # a) numeric vectors
  beta_res <- CAPM.beta(Ra, Rb)
  expect_true(is.numeric(beta_res))
  
  # Check against manual regression (formula: cov(Ra, Rb) / var(Rb))
  # wait, cov(Ra, Rb) / var(Rb) is only true if Rf=0. By default CAPM.beta uses Rf=0.
  beta_manual <- cov(Ra, Rb) / var(Rb)
  expect_equal(as.numeric(beta_res), beta_manual, tolerance = 1e-6)
})

test_that("CAPM.beta handles zoo objects with Date index correctly", {
  skip_on_cran()
  
  set.seed(42)
  dates <- as.Date("2020-01-01") + 1:50
  Ra_zoo <- zoo::zoo(rnorm(50, mean=0.02), dates)
  Rb_zoo <- zoo::zoo(rnorm(50, mean=0.01), dates)
  
  # b) zoo objects with Date indices
  beta_zoo <- CAPM.beta(Ra_zoo, Rb_zoo)
  expect_true(is.numeric(beta_zoo))
  
  beta_manual_zoo <- cov(Ra_zoo, Rb_zoo) / var(Rb_zoo)
  expect_equal(as.numeric(beta_zoo), as.numeric(beta_manual_zoo), tolerance = 1e-6)
})
