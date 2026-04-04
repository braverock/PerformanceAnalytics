library(PerformanceAnalytics)
library(xts)

test_that("Return.portfolio covers baseline features before modifications", {
  skip_on_cran()
  data(edhec)
  
  # A. Test arithmetic returns with wealth.index AND verbose
  R_subset <- edhec[1:5, 1:3]
  weights <- c(0.4, 0.4, 0.2)
  
  res_arith_verb <- Return.portfolio(R_subset, weights = weights, geometric = FALSE, wealth.index = TRUE, verbose = TRUE)
  expect_true(is.list(res_arith_verb))
  expect_equal(length(res_arith_verb), 5)
  expect_true(!is.null(res_arith_verb$wealthindex))
  
  # B. Test warning states on weights missing names/columns
  weights_missing <- xts(matrix(c(0.4, 0.6), nrow=1), order.by=as.Date("1996-12-31"))
  # This expects a warning because 2 assets in weights vs 3 assets in R
  expect_warning(Return.portfolio(R_subset, weights = weights_missing), "number of assets in beginning_weights is less than number of columns in returns")
  
  # C. Test weights names alignment mapping
  colnames(weights_missing) <- c("Convertible Arbitrage", "Distressed Securities")
  res_aligned <- Return.portfolio(R_subset, weights = weights_missing)
  # R_subset columns are Convertible Arbitrage, CTA Global, Distressed Securities
  # By using names, it drops CTA Global and operates on the matching columns
  expect_equal(dim(res_aligned)[2], 1)
  
  # D. Rebalance_on warnings
  # This ensures rebalancing works gracefully without explicitly dropping time indexes
  expect_error(res_reb <- Return.portfolio(R_subset, weights = weights, rebalance_on = "months"), NA)
  expect_equal(dim(res_reb)[1], 5)
})
