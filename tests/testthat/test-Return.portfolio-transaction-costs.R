library(PerformanceAnalytics)
library(xts)

test_that("Return.portfolio calculates proportional transaction costs (fixes #182)", {
  skip_on_cran()
  
  # Simple 2-period, 2-asset universe
  R <- xts(matrix(c(0.1, 0.1, 
                    0.2, 0.2), ncol=2, byrow=TRUE), 
           order.by=as.Date(c("2020-01-31", "2020-02-29")))
  colnames(R) <- c("A", "B")
  
  # Weights rebalance exactly at end of Jan (take effect in Feb)
  w <- xts(matrix(c(0.5, 0.5,
                    0.6, 0.4), ncol=2, byrow=TRUE),
           order.by=as.Date(c("1999-12-31", "2020-01-31")))
  colnames(w) <- c("A", "B")
  
  # Baseline calculation (no costs)
  res_no_cost <- Return.portfolio(R, weights=w, rebal_cost=0, verbose=TRUE)
  
  # Transaction Cost calculation
  cost_pct <- 0.01 # 1% transaction cost
  res_cost <- Return.portfolio(R, weights=w, rebal_cost=cost_pct, verbose=TRUE)
  
  # A. In Jan, there is NO rebalance happening *during* the month
  # So Jan returns should be identical
  expect_equal(as.numeric(res_cost$returns[1]), as.numeric(res_no_cost$returns[1]))
  
  # B. In Feb, the portfolio rebalances from drifted weights to the new 60/40 targets.
  
  # Jan drifted weights (EOP weights)
  w_drift_A <- as.numeric(res_no_cost$EOP.Weight[1, 1])
  w_drift_B <- as.numeric(res_no_cost$EOP.Weight[1, 2])
  
  # We want to rebalance to 0.6 and 0.4
  trade_A <- 0.6 - w_drift_A
  trade_B <- 0.4 - w_drift_B
  
  total_turnover <- abs(trade_A) + abs(trade_B)
  expected_drag <- total_turnover * cost_pct
  
  # Because the cost reduces the capital entering the second period, 
  # the new BOP Value is scaled down by (1 - expected_drag)
  expect_equal(as.numeric(sum(res_cost$BOP.Value[2, ])), as.numeric(sum(res_no_cost$BOP.Value[2, ])) * (1 - expected_drag))
  
  # C. Test that the total portfolio return in Feb is reduced mathematically
  diff <- as.numeric(res_no_cost$returns[2] - res_cost$returns[2])
  expect_true(diff > 0) # Return must be smaller!
  # Drag applies against the entering capital, which misses out on the 20% gain it would have generated!
  expect_equal(diff, expected_drag * (1 + 0.2)) 
  
  # D. Validate Verbose Output Components 
  # Check that contributions natively subtract their fractional proportion of the drag
  expect_equal(as.numeric(sum(res_cost$contribution[2, ])), as.numeric(res_cost$returns[2]))
  expect_true(as.numeric(res_cost$contribution[2, 1]) < as.numeric(res_no_cost$contribution[2, 1]))
  expect_true(as.numeric(res_cost$contribution[2, 2]) < as.numeric(res_no_cost$contribution[2, 2]))
  
  # E. Test edge cases with dropping assets
  # If an asset drops to zero, the drag should STILL affect its contribution for the period it was sold
  w_drop <- xts(matrix(c(0.5, 0.5,
                         1.0, 0.0), ncol=2, byrow=TRUE),
           order.by=as.Date(c("1999-12-31", "2020-01-31")))
           
  res_cost_drop <- Return.portfolio(R, weights=w_drop, rebal_cost=cost_pct, verbose=TRUE)
  
  # Asset B dropped to 0 weight for Feb. Its BOP Value is 0, so its raw return is 0. 
  # BUT it incurred a transaction cost to sell off its entire drifted weight!
  # Therefore, its contribution for Feb should be literally negative!
  expect_true(as.numeric(res_cost_drop$contribution[2, 2]) < 0)
  
  # Ensure the portfolio returns match the sum of contributions
  expect_equal(as.numeric(sum(res_cost_drop$contribution[2, ])), as.numeric(res_cost_drop$returns[2]))
})
