library(PerformanceAnalytics)
library(xts)

test_that("Table scale explicitly passed skips periodicity label", {
  skip_on_cran()
  
  data(edhec)
  R <- edhec[, 1:2]
  
  # 1. Distributions
  res_dist_default <- table.Distributions(R)
  res_dist_scale <- table.Distributions(R, scale=12)
  
  expect_true(any(grepl("monthly", rownames(res_dist_default))))
  expect_false(any(grepl("monthly", rownames(res_dist_scale))))
  expect_true("Std Dev" %in% rownames(res_dist_scale))
  
  # 2. DownsideRiskRatio
  res_down_default <- table.DownsideRiskRatio(R)
  res_down_scale <- table.DownsideRiskRatio(R, scale=12)
  
  expect_true(any(grepl("monthly", rownames(res_down_default))))
  expect_false(any(grepl("monthly", rownames(res_down_scale))))
  expect_true("Downside risk" %in% rownames(res_down_scale))
  
  # 3. Variability
  res_var_default <- table.Variability(R)
  res_var_scale <- table.Variability(R, scale=12)
  
  expect_true(any(grepl("monthly", rownames(res_var_default))))
  expect_false(any(grepl("monthly", rownames(res_var_scale))))
  expect_true("Std Dev" %in% rownames(res_var_scale))
  
  # Ensure the values are identical whether passed manually or via periodicity deduction
  expect_equal(res_dist_default, res_dist_scale, ignore_attr=TRUE)
  expect_equal(res_down_default, res_down_scale, ignore_attr=TRUE)
  expect_equal(res_var_default, res_var_scale, ignore_attr=TRUE)
})
