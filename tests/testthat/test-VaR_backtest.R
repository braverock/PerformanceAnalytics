library(PerformanceAnalytics)
library(xts)

test_that("VaR.backtest evaluates actual vs expected breaches accurately", {
  skip_on_cran()
  data(edhec)
  
  # A. Use a historical VaR threshold on a single column
  R <- edhec[, 1, drop=FALSE]
  
  # Historical VaR at 95% should naturally breach roughly 5% of the time in sample
  v_hist <- as.numeric(VaR(R, p=0.95, method="historical"))
  
  res_hist <- VaR.backtest(R, v_hist, p=0.95)
  
  expect_equal(res_hist$expected_exceedances, length(R) * 0.05)
  # Assert actuals count matches the manual calculation
  expect_equal(res_hist$actual_exceedances, sum(R < v_hist, na.rm=TRUE))
  
  # The binomial proportion test should return a p-value for the historical in-sample test
  expect_true(is.numeric(res_hist$p.value))
  expect_true(res_hist$p.value >= 0 && res_hist$p.value <= 1)
  
  # B. Test a deliberate failure case (terrible VaR model)
  # If our VaR threshold is far too low (e.g. -0.20 when the market barely moves), we should see zero breaches
  v_bad <- -0.50
  res_bad <- VaR.backtest(R, v_bad, p=0.95)
  
  expect_equal(res_bad$actual_exceedances, 0)
  
  # C. Test a vector VaR (time-varying risk threshold)
  # For this synthetic test, we'll just drift the VaR artificially
  v_vector <- seq(v_hist * 0.8, v_hist * 1.2, length.out = length(R))
  res_vec <- VaR.backtest(R, v_vector, p=0.95)
  
  expect_equal(res_vec$actual_exceedances, sum(R < v_vector, na.rm=TRUE))
  expect_true(is.numeric(res_vec$p.value))
})
