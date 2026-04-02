library(PerformanceAnalytics)
library(xts)

test_that("SharpeRatio uses excess-return standard deviation (fixes #142)", {
  skip_on_cran()
  
  data(managers)
  R <- managers[, 1, drop=FALSE]
  Rf_scalar <- 0.035/12
  Rf_vec <- managers[, 10, drop=FALSE]
  
  # 1. Test scalar Rf (Should be mathematically identical to old behavior because variance is shift-invariant)
  old_sr_scalar <- Return.annualized(Return.excess(R, Rf_scalar)) / StdDev.annualized(R)
  new_sr_scalar <- Return.annualized(Return.excess(R, Rf_scalar)) / StdDev.annualized(Return.excess(R, Rf_scalar))
  
  expect_equal(as.numeric(old_sr_scalar), as.numeric(new_sr_scalar))
  
  # Function output matches new correct formula exactly
  out_sr_scalar <- SharpeRatio.annualized(R, Rf = Rf_scalar)
  expect_equal(as.numeric(out_sr_scalar), as.numeric(new_sr_scalar))
  
  # 2. Test vector Rf (Time-varying Rf causes R and xR to have different standard deviations!)
  old_sr_vec <- Return.annualized(Return.excess(R, Rf_vec)) / StdDev.annualized(R)
  new_sr_vec <- Return.annualized(Return.excess(R, Rf_vec)) / StdDev.annualized(Return.excess(R, Rf_vec))
  
  # Ensure they are mathematically divergent
  expect_false(isTRUE(all.equal(as.numeric(old_sr_vec), as.numeric(new_sr_vec))))
  
  # Function output MUST match new correct formula exactly, not the old broken one
  out_sr_vec <- SharpeRatio.annualized(R, Rf = Rf_vec)
  expect_equal(as.numeric(out_sr_vec), as.numeric(new_sr_vec))
  
  # Also test standard non-annualized SharpeRatio function
  out_monthly_vec <- SharpeRatio(R, Rf = Rf_vec, FUN="StdDev")
  
  # Manual calculation
  xR <- Return.excess(R, Rf_vec)
  new_monthly_vec <- mean(xR, na.rm=TRUE) / sd(xR, na.rm=TRUE)
  
  expect_equal(as.numeric(out_monthly_vec), as.numeric(new_monthly_vec))
})
