library(PerformanceAnalytics)
library(xts)

test_that("Pre-existing VaR and ES methods calculate consistently (Baseline Test)", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:100, 1:3]
  w <- c(0.3, 0.4, 0.3)
  
  # Ensure univariate exact matches (invert=FALSE natively by default)
  res_gaussian <- as.numeric(VaR(R[,1], method="gaussian", p=0.95))
  res_modified <- as.numeric(VaR(R[,1], method="modified", p=0.95))
  res_historical <- as.numeric(VaR(R[,1], method="historical", p=0.95))
  
  expect_equal(round(res_gaussian, 6), -0.012133)
  expect_equal(round(res_modified, 6), -0.014674)
  expect_equal(round(res_historical, 6), -0.014095)
  
  # Ensure multivariate component exact matches
  res_comp_gauss <- as.numeric(VaR(R, method="gaussian", portfolio_method="component", weights=w, p=0.95)$VaR)
  
  expect_equal(round(res_comp_gauss, 6), 0.012916)
  
  # ES calculations
  es_gaussian <- as.numeric(ES(R[,1], method="gaussian", p=0.95))
  es_historical <- as.numeric(ES(R[,1], method="historical", p=0.95))
  
  expect_equal(round(es_gaussian, 6), -0.017133)
  expect_equal(round(es_historical, 6), -0.024080)
})
