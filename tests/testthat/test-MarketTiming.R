library(PerformanceAnalytics)
library(xts)

test_that("MarketTiming Henriksson-Merton regressor uses correct sign (fixes #2)", {
  skip_on_cran()
  
  set.seed(42)
  xRb <- rnorm(100, mean=0.005, sd=0.05)
  # Perfect timer: Rp - Rf = max(Rb - Rf, 0)
  # Which equals (Rb - Rf) + max(0, Rf - Rb)
  # So Beta = 1, Gamma = 1
  xRa <- pmax(xRb, 0)
  
  Ra <- xts(xRa, order.by=as.Date("2020-01-01") + 1:100)
  Rb <- xts(xRb, order.by=as.Date("2020-01-01") + 1:100)
  colnames(Ra) <- "Manager"
  colnames(Rb) <- "Market"
  
  res <- MarketTiming(Ra, Rb, method="HM")
  
  expect_equal(as.numeric(res[1, "Beta"]), 1, tolerance = 1e-10)
  expect_equal(as.numeric(res[1, "Gamma"]), 1, tolerance = 1e-10)
  expect_equal(as.numeric(res[1, "Alpha"]), 0, tolerance = 1e-10)
})

test_that("MarketTiming Treynor-Mazuy regressor estimates correctly", {
  skip_on_cran()
  
  set.seed(42)
  xRb <- rnorm(100, mean=0.005, sd=0.05)
  # Quadratic timing: Rp - Rf = 0.01 + 0.8 * (Rb - Rf) + 2.0 * (Rb - Rf)^2
  xRa <- 0.01 + 0.8 * xRb + 2.0 * xRb^2
  
  Ra <- xts(xRa, order.by=as.Date("2020-01-01") + 1:100)
  Rb <- xts(xRb, order.by=as.Date("2020-01-01") + 1:100)
  colnames(Ra) <- "Manager"
  colnames(Rb) <- "Market"
  
  res <- MarketTiming(Ra, Rb, method="TM")
  
  expect_equal(as.numeric(res[1, "Alpha"]), 0.01, tolerance = 1e-10)
  expect_equal(as.numeric(res[1, "Beta"]), 0.8, tolerance = 1e-10)
  expect_equal(as.numeric(res[1, "Gamma"]), 2.0, tolerance = 1e-10)
})
