library(PerformanceAnalytics)
library(xts)

test_that("Return.portfolio supports vector transaction costs", {
  skip_on_cran()

  R <- xts(
    matrix(c(
      0.1, 0.1,
      0.2, 0.2
    ), ncol = 2, byrow = TRUE),
    order.by = as.Date(c("2020-01-31", "2020-02-29"))
  )
  colnames(R) <- c("A", "B")

  w <- xts(
    matrix(c(
      0.5, 0.5,
      0.6, 0.4
    ), ncol = 2, byrow = TRUE),
    order.by = as.Date(c("1999-12-31", "2020-01-31"))
  )
  colnames(w) <- c("A", "B")

  res_no_cost <- Return.portfolio(R, weights = w, rebal_cost = 0, verbose = TRUE)

  # Transaction Cost Calculation
  # Asset A has 0% cost, Asset B has 2% cost
  cost_vec <- c(0.00, 0.02)
  res_cost <- Return.portfolio(R, weights = w, rebal_cost = cost_vec, verbose = TRUE)

  # A. Validate exceptions fire for improperly sized cost vectors
  expect_error(Return.portfolio(R, weights = w, rebal_cost = c(0.01, 0.01, 0.01)))

  # B. Let's manually calculate the expected drag.
  w_drift_A <- as.numeric(res_no_cost$EOP.Weight[1, 1])
  w_drift_B <- as.numeric(res_no_cost$EOP.Weight[1, 2])

  trade_A <- 0.6 - w_drift_A
  trade_B <- 0.4 - w_drift_B

  # Expected drag is ONLY driven by the transaction volume of Asset B since A is free!
  expected_drag <- (abs(trade_A) * cost_vec[1]) + (abs(trade_B) * cost_vec[2])

  # Test the BOP value correctly reduced by the precise mixed cost vector
  expect_equal(as.numeric(sum(res_cost$BOP.Value[2, ])), as.numeric(sum(res_no_cost$BOP.Value[2, ])) * (1 - expected_drag))

  # Test the overall portfolio return
  diff <- as.numeric(res_no_cost$returns[2] - res_cost$returns[2])
  expect_equal(diff, expected_drag * (1 + 0.2))

  # Test verbose contributions correctly allocate costs natively
  # Because Asset A has 0% cost, its *direct* cost_allocation subtraction is 0.
  # However, its contribution is mathematically derived from the *total* portfolio
  # value which was slightly scaled down by the drag!
  # Therefore, its contribution still mathematically scales down by exactly
  # the proportion of the drag applied to the portfolio's entering capital.

  # Expected scaling formula: (returns * bop_value - cost_allocation) / prior_end_value

  expect_equal(as.numeric(res_cost$contribution[2, 1]), as.numeric(res_no_cost$contribution[2, 1]) * (1 - expected_drag))

  # Asset B's contribution difference absorbs the ENTIRE absolute cost drag natively
  # PLUS the proportional scale down of the remaining portfolio capital.
  expect_equal(as.numeric(sum(res_cost$contribution[2, ])), as.numeric(res_cost$returns[2]))
  expect_true(as.numeric(res_cost$contribution[2, 2]) < as.numeric(res_no_cost$contribution[2, 2]))
})
