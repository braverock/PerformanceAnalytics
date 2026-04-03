library(PerformanceAnalytics)
library(xts)

test_that("Return.portfolio gracefully handles weights that start after the return series (fixes #83)", {
  skip_on_cran()
  
  dates <- as.Date("2020-01-01") + 0:4
  R <- xts(matrix(c(
    0.10, 0.05, -0.05, 0.10, 0.05,
    -0.05, 0.05, 0.10, -0.10, 0.02
  ), ncol=2), order.by=dates)
  colnames(R) <- c("A", "B")
  
  # Staggered start: weights provided on 2020-01-03
  w_xts <- xts(matrix(c(0.6, 0.4), nrow=1), order.by=as.Date("2020-01-03"))
  colnames(w_xts) <- c("A", "B")
  
  # Current behavior: this subsets R to start after 2020-01-03, effectively dropping 2020-01-01 to 2020-01-03!
  # It should warn the user, insert an equal-weight row at the start of R, and evaluate the full history.
  expect_warning({
    res <- Return.portfolio(R, weights=w_xts)
  }, "First weights period is after the start of the data series")
  
  # Assert that the full timeline was preserved and evaluated
  expect_equal(nrow(res), nrow(R))
  
  # The first period should be evaluated using equal weights (the default fallback)
  expect_equal(as.numeric(res[1, ]), 0.5 * 0.10 + 0.5 * -0.05)
})
