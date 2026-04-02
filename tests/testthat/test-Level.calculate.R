library(PerformanceAnalytics)
library(xts)

test_that("Level.calculate is exported and available", {
  skip_on_cran()
  expect_true("Level.calculate" %in% getNamespaceExports("PerformanceAnalytics"))
})

test_that("Level.calculate computes implied start dates properly", {
  skip_on_cran()
  
  data(managers)
  R <- managers[2:5, 1, drop=FALSE] # Start at second row so it doesn't have an NA
  xtsAttributes(R) <- list(coredata_content = "discreteReturn")
  
  expect_warning({
    res <- Level.calculate(R, seedValue = 100)
  }, "Estimated start date/time")
  
  expect_equal(nrow(res), nrow(R) + 1)
  expect_equal(as.numeric(res[1,1]), 100)
})

test_that("Level.calculate handles discreteReturn correctly", {
  skip_on_cran()
  
  data(prices)
  prices_xts <- as.xts(prices)
  R <- Return.calculate(prices_xts, method="discrete")
  
  # Return.calculate with xts natively sets coredata_content = "discreteReturn"
  res <- Level.calculate(R, seedValue = 1)
  cum_ret <- Return.cumulative(R)
  
  expect_equal(as.numeric(tail(res[,1] - 1, 1)), as.numeric(cum_ret[,1]))
})

test_that("Level.calculate handles logReturn correctly", {
  skip_on_cran()
  
  data(prices)
  prices_xts <- as.xts(prices)
  R_log <- Return.calculate(prices_xts, method="log")
  
  res_log <- Level.calculate(R_log, seedValue = 1)
  
  # Verify log return calculation baseline
  manual_log <- exp(cumsum(na.omit(R_log[,1])))
  expect_equal(as.numeric(tail(res_log[,1], 1)), as.numeric(tail(manual_log, 1)))
})

test_that("Level.calculate handles difference correctly", {
  skip_on_cran()
  
  data(prices)
  prices_xts <- as.xts(prices)
  R_diff <- Return.calculate(prices_xts, method="difference")
  
  res_diff <- Level.calculate(R_diff, seedValue = as.numeric(prices_xts[1,1]))
  
  expect_equal(as.numeric(tail(res_diff[,1], 1)), as.numeric(tail(prices_xts[,1], 1)))
})

test_that("Level.calculate respects initial=FALSE flag", {
  skip_on_cran()
  
  data(prices)
  prices_xts <- as.xts(prices)
  R <- Return.calculate(prices_xts, method="discrete")
  
  res_rev <- Level.calculate(R, seedValue = 100, initial = FALSE)
  
  expect_equal(nrow(res_rev), nrow(R))
  expect_equal(as.numeric(tail(res_rev[,1], 1)), 100)
})

test_that("Level.calculate handles missing attribute gracefully", {
  skip_on_cran()
  
  data(prices)
  prices_xts <- as.xts(prices)
  R <- Return.calculate(prices_xts, method="discrete")
  attr(R, "coredata_content") <- NULL
  
  expect_warning({
      res <- Level.calculate(R, seedValue = 100)
  }, "object is missing coredata_content")
  
  expect_true(is.xts(res))
})

test_that("Level.calculate throws error for invalid input", {
  skip_on_cran()
  
  data(prices)
  prices_xts <- as.xts(prices)
  R <- Return.calculate(prices_xts, method="discrete")
  xtsAttributes(R)$coredata_content <- "invalid"
  
  expect_error(Level.calculate(R), "Unknown coredata_content attribute")
  suppressWarnings(expect_error(Level.calculate(as.numeric(R)), "Must pass an xts object"))
})
