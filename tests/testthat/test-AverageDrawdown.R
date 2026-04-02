library(PerformanceAnalytics)
library(xts)

test_that("AverageDrawdown pipeline natively preserves xts structures without apply coercion crashes", {
  skip_on_cran()
  
  # Simulate Yahoo Finance / quantmod output (xts with Date index)
  dates <- as.Date("2020-01-01") + 1:100
  
  # Create a synthetic dataset mimicking SP500 and BVSP
  set.seed(123)
  R_sp500 <- rnorm(100, mean=0.0005, sd=0.01)
  R_bvsp <- rnorm(100, mean=0.001, sd=0.02)
  
  R <- xts(cbind(R_sp500, R_bvsp), order.by = dates)
  colnames(R) <- c("SP500", "BVSP")
  
  # Inject NAs to mimic the user's report ("BVSP contains missing values...")
  R[c(10, 50, 99), 2] <- NA
  
  # Test AverageDrawdown
  res_ad <- expect_error(AverageDrawdown(R), NA)
  expect_true(is.matrix(res_ad))
  expect_equal(dim(res_ad), c(1, 2))
  expect_equal(rownames(res_ad), "Average Drawdown")
  
  # Test AverageLength
  res_al <- expect_error(AverageLength(R), NA)
  expect_true(is.matrix(res_al))
  expect_equal(dim(res_al), c(1, 2))
  expect_equal(rownames(res_al), "Average Length")
  
  # Test AverageRecovery
  res_ar <- expect_error(AverageRecovery(R), NA)
  expect_true(is.matrix(res_ar))
  expect_equal(dim(res_ar), c(1, 2))
  expect_equal(rownames(res_ar), "Average Recovery")
  
  # Verify that non-parseable zoo integer arrays succeed without checkData breaking
  z <- zoo::zoo(matrix(1:10, ncol=2), 1:5)
  res_z <- suppressWarnings(expect_error(AverageDrawdown(z), NA))
  expect_true(is.matrix(res_z))
})
