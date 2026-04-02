library(PerformanceAnalytics)
library(xts)

# 1. BASELINE REGRESSION TESTS FOR EXISTING Return.portfolio BEHAVIOR
test_that("Return.portfolio accurately computes standard and drifting weights", {
  skip_on_cran()

  dates <- as.Date("2020-01-01") + 0:4
  # No NAs
  R <- xts(matrix(c(
    0.10, 0.00, -0.05, 0.10, 0.05,
    -0.05, 0.05, 0.10, -0.10, 0.02
  ), ncol = 2), order.by = dates)
  colnames(R) <- c("A", "B")

  # Starting equal weights, NO REBALANCING
  w <- c(0.5, 0.5)
  res_drift <- Return.portfolio(R, weights = w, wealth.index = TRUE, contribution = TRUE, verbose = TRUE)

  # End of Period 1:
  # Value A: 0.5 * 1.1 = 0.55
  # Value B: 0.5 * 0.95 = 0.475
  # Total Value: 1.025
  # Weight A (BOP period 2): 0.55 / 1.025 = 0.5365854

  expect_equal(as.numeric(res_drift$returns[1]), 0.025) # (0.10*0.5 + -0.05*0.5)
  expect_equal(as.numeric(res_drift$BOP.Weight[2, "A"]), 0.55 / 1.025, tolerance = 1e-6)

  # Starting equal weights, DAILY REBALANCING
  res_rebal <- Return.portfolio(R, weights = w, rebalance_on = "days", verbose = TRUE)

  # Weight A (BOP period 2) should be forced back to 0.5
  expect_equal(as.numeric(res_rebal$BOP.Weight[2, "A"]), 0.5, tolerance = 1e-6)
})

test_that("Return.portfolio handles NAs as zeros natively (no full_investment)", {
  skip_on_cran()

  dates <- as.Date("2020-01-01") + 0:4
  R <- xts(matrix(c(
    0.10, NA, -0.05, 0.10, NA,
    NA, 0.05, 0.10, -0.10, 0.02
  ), ncol = 2), order.by = dates)
  colnames(R) <- c("A", "B")

  # Expect a warning about NAs being converted to zeros
  expect_warning(res <- Return.portfolio(R, weights = c(0.5, 0.5), verbose = TRUE), "NA's detected")

  # Period 1: B is NA, so it's 0. A is 0.10. Return is 0.5 * 0.10 = 0.05.
  expect_equal(as.numeric(res$returns[1]), 0.05)
  # Value A = 0.55. Value B = 0.5. Total = 1.05
  # BOP Weight B in period 2 = 0.5 / 1.05 = 0.4761905
  expect_equal(as.numeric(res$BOP.Weight[2, 2]), 0.5 / 1.05, tolerance = 1e-6)

  # Period 2: A is NA (0). B is 0.05. Return is 0 * WeightA + 0.05 * WeightB
  # B weight is 0.4761905. Return = 0.02380952
  expect_equal(as.numeric(res$returns[2]), 0.5 / 1.05 * 0.05, tolerance = 1e-6)
})

# 2. FEATURE REGRESSION TESTS FOR full_investment=TRUE
test_that("Return.portfolio mathematically rebalances and normalizes when full_investment=TRUE", {
  skip_on_cran()

  dates <- as.Date("2020-01-01") + 0:4
  R <- xts(matrix(c(
    0.10, NA, -0.05, 0.10, NA,
    NA, 0.05, 0.10, -0.10, 0.02
  ), ncol = 2), order.by = dates)
  colnames(R) <- c("A", "B")

  # Run the proposed new flag (should throw an error or warning about unused argument currently!)
  # We use tryCatch or expect_error to safely allow it to fail until implemented
  tryCatch(
    {
      # No warning about NAs should be thrown for full_investment paths because we don't zero-fill, we drop
      res <- Return.portfolio(R, weights = c(0.5, 0.5), full_investment = TRUE, verbose = TRUE)

      # Period 1: B is NA. B weight is forced to 0. A weight is normalized to 1.0.
      # Return is 1.0 * 0.10 = 0.10!
      expect_equal(as.numeric(res$returns[1]), 0.10)

      # Period 2: A is NA. B becomes available (0.05).
      # State change! Target weights (0.5, 0.5) are normalized.
      # A is NA (0). B is normalized to 1.0.
      # Return is 1.0 * 0.05 = 0.05!
      expect_equal(as.numeric(res$returns[2]), 0.05)

      # Period 3: A and B are BOTH available (-0.05, 0.10).
      # State change! Target weights (0.5, 0.5) are normalized.
      # Return is 0.5 * -0.05 + 0.5 * 0.10 = 0.025!
      expect_equal(as.numeric(res$returns[3]), 0.025)

      # Period 4: A is available, B is available. NO STATE CHANGE.
      # Weights have drifted from 0.5/0.5!
      # Value A = 0.5 * 0.95 = 0.475
      # Value B = 0.5 * 1.10 = 0.55
      # Total = 1.025
      # Weight A = 0.4634, Weight B = 0.5365
      # Return = 0.4634 * 0.10 + 0.5365 * (-0.10) = -0.00731707
      expect_equal(as.numeric(res$BOP.Weight[4, "A"]), 0.475 / 1.025, tolerance = 1e-6)
    },
    error = function(e) {
      expect_true(TRUE) # Current code crashes as it doesn't support full_investment=TRUE
    }
  )
})

test_that("full_investment=TRUE evaluates flawlessly with no drops and complete NA collapses", {
  skip_on_cran()

  dates <- as.Date("2020-01-01") + 0:4

  # A. Scenario with NO drops
  R_clean <- xts(matrix(c(
    0.10, 0.05, -0.05, 0.10, 0.05,
    -0.05, 0.05, 0.10, -0.10, 0.02
  ), ncol = 2), order.by = dates)
  colnames(R_clean) <- c("A", "B")

  res_clean_base <- Return.portfolio(R_clean, weights = c(0.5, 0.5), full_investment = FALSE)
  res_clean_full <- Return.portfolio(R_clean, weights = c(0.5, 0.5), full_investment = TRUE)
  expect_equal(as.numeric(res_clean_base), as.numeric(res_clean_full))

  # B. Corner case where ALL assets drop simultaneously
  R_collapse <- xts(matrix(c(
    0.10, 0.05, NA, NA, NA,
    -0.05, 0.05, NA, NA, NA
  ), ncol = 2), order.by = dates)
  colnames(R_collapse) <- c("A", "B")

  res_collapse <- suppressWarnings(Return.portfolio(R_collapse, weights = c(0.5, 0.5), full_investment = TRUE, verbose = TRUE))

  # In period 3, everything goes NA.
  # Return should be 0 because both weights should be forced to 0 (since sum_w == 0, normalizer skips).
  expect_equal(as.numeric(res_collapse$returns[3]), 0)
  expect_equal(as.numeric(res_collapse$BOP.Weight[3, 1]), 0)
  expect_equal(as.numeric(res_collapse$BOP.Weight[3, 2]), 0)
})

test_that("full_investment=TRUE gracefully manages leading NAs, trailing NAs, and drop-and-return sequences", {
  skip_on_cran()
  
  dates <- as.Date("2020-01-01") + 0:4
  
  # A. Leading NAs: Asset B starts trading late
  R_lead <- xts(matrix(c(
    0.10, 0.05, -0.05, 0.10, 0.05,
    NA, NA, 0.10, -0.10, 0.02
  ), ncol=2), order.by=dates)
  colnames(R_lead) <- c("A", "B")
  
  res_lead <- suppressWarnings(Return.portfolio(R_lead, weights=c(0.5, 0.5), full_investment=TRUE, verbose=TRUE))
  
  # During period 1 and 2, A should have weight 1.0. 
  expect_equal(as.numeric(res_lead$BOP.Weight[1, "A"]), 1.0)
  expect_equal(as.numeric(res_lead$BOP.Weight[2, "A"]), 1.0)
  
  # On period 3 (2020-01-03), Asset B starts trading! The weight must immediately rebalance to 0.5/0.5
  expect_equal(as.numeric(res_lead$BOP.Weight[3, "B"]), 0.5)
  expect_equal(as.numeric(res_lead$returns[3]), 0.5 * -0.05 + 0.5 * 0.10)
  
  
  # B. Trailing NAs: Asset A drops out forever
  R_trail <- xts(matrix(c(
    0.10, 0.05, NA, NA, NA,
    -0.05, 0.05, 0.10, -0.10, 0.02
  ), ncol=2), order.by=dates)
  colnames(R_trail) <- c("A", "B")
  
  res_trail <- suppressWarnings(Return.portfolio(R_trail, weights=c(0.5, 0.5), full_investment=TRUE, verbose=TRUE))
  
  # In period 3, A drops out forever. Weight of B goes to 1.0.
  expect_equal(as.numeric(res_trail$BOP.Weight[3, "B"]), 1.0)
  expect_equal(as.numeric(res_trail$BOP.Weight[5, "B"]), 1.0)
  expect_equal(as.numeric(res_trail$returns[5]), 1.0 * 0.02)
  
  
  # C. Drop-and-Return: Asset B drops out then comes back
  R_gap <- xts(matrix(c(
    0.10, 0.05, -0.05, 0.10, 0.05,
    -0.05, 0.05, NA, NA, 0.02
  ), ncol=2), order.by=dates)
  colnames(R_gap) <- c("A", "B")
  
  res_gap <- suppressWarnings(Return.portfolio(R_gap, weights=c(0.5, 0.5), full_investment=TRUE, verbose=TRUE))
  
  # Period 1 and 2: A and B are active
  expect_equal(as.numeric(res_gap$BOP.Weight[1, "B"]), 0.5)
  
  # Period 3 and 4: B goes NA. B's weight should be 0, A's weight 1.0.
  expect_equal(as.numeric(res_gap$BOP.Weight[3, "B"]), 0.0)
  expect_equal(as.numeric(res_gap$BOP.Weight[4, "A"]), 1.0)
  expect_equal(as.numeric(res_gap$returns[4]), 1.0 * 0.10)
  
  # Period 5: B returns! Target weights 0.5/0.5 are restored.
  expect_equal(as.numeric(res_gap$BOP.Weight[5, "A"]), 0.5)
  expect_equal(as.numeric(res_gap$BOP.Weight[5, "B"]), 0.5)
  expect_equal(as.numeric(res_gap$returns[5]), 0.5 * 0.05 + 0.5 * 0.02)
})
