library(PerformanceAnalytics)
library(xts)

test_that("CDD (Conditional Drawdown at Risk) evaluates distinct mathematical properties across methods (fixes #75)", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1, drop=FALSE]
  
  # A. Legacy Quantile (VaR of drawdowns)
  res_quant <- suppressWarnings(CDD(R, method="quantile", p=0.95))
  
  # Manual calculation
  dd_all <- sortDrawdowns(findDrawdowns(na.omit(R)))
  p_val <- 0.05 # inverted threshold for 0.95
  man_quant <- -quantile(dd_all$return, p_val) # inverted
  
  expect_equal(as.numeric(res_quant), as.numeric(man_quant))
  
  # B. Discrete Mean (ES of drawdowns)
  res_disc <- CDD(R, method="discrete", p=0.95)
  man_disc <- -mean(dd_all$return[dd_all$return <= -as.numeric(man_quant)])
  expect_equal(as.numeric(res_disc), as.numeric(man_disc))
  
  # Expected shortfall of negative numbers MUST be strictly greater or equal to the quantile boundary 
  # Since inverted, res_disc >= res_quant
  expect_true(as.numeric(res_disc) >= as.numeric(res_quant))
  
  # C. Continuous Average (Sandbox reference method)
  res_avg <- CDD(R, method="average", p=0.95)
  dd_cont <- coredata(Drawdowns(na.omit(R)))
  dar <- quantile(dd_cont, p_val, type=8)
  man_avg <- -mean(dd_cont[dd_cont <= dar])
  
  expect_equal(as.numeric(res_avg), as.numeric(man_avg))
})
