library(PerformanceAnalytics)
library(xts)

test_that("charts.PerformanceSummary plot engines work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:2]
  
  # A. default
  expect_error(charts.PerformanceSummary(R, plot.engine="default"), NA)
  
  # B. ggplot2
  if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("gridExtra", quietly = TRUE)) {
    expect_error(res_gg <- charts.PerformanceSummary(R, plot.engine="ggplot2"), NA)
    expect_s3_class(res_gg, "gtable")
  }
  
  # C. plotly
  if (requireNamespace("plotly", quietly = TRUE)) {
    expect_error(res_pl <- charts.PerformanceSummary(R, plot.engine="plotly"), NA)
    expect_s3_class(res_pl, "plotly")
  }
  
  # D. invalid engine fallback warning
  expect_warning(res_bad <- charts.PerformanceSummary(R, plot.engine="invalid"), "Ploting chart using built-in engine now")
  expect_s3_class(res_bad, "recordedplot")
})

test_that("charts.PerformanceSummary parses frequency paths", {
  skip_on_cran()
  
  set.seed(42)
  # minute
  m_idx <- as.POSIXct("2020-01-01 09:30:00") + 60 * 1:10
  R_m <- xts(rnorm(10, 0, 0.01), m_idx)
  expect_error(charts.PerformanceSummary(R_m), NA)
  
  # hourly
  h_idx <- as.POSIXct("2020-01-01 09:00:00") + 3600 * 1:10
  R_h <- xts(rnorm(10, 0, 0.01), h_idx)
  expect_error(charts.PerformanceSummary(R_h), NA)
  
  # daily
  d_idx <- as.Date("2020-01-01") + 1:10
  R_d <- xts(rnorm(10, 0, 0.01), d_idx)
  expect_error(charts.PerformanceSummary(R_d), NA)
  
  # weekly
  w_idx <- as.Date("2020-01-01") + 7 * 1:10
  R_w <- xts(rnorm(10, 0, 0.01), w_idx)
  expect_error(charts.PerformanceSummary(R_w), NA)
  
  # quarterly
  q_idx <- as.Date(paste0("2020-0", c(1,4,7), "-01"))
  R_q <- xts(rnorm(4, 0, 0.01), c(q_idx, as.Date("2020-10-01")))
  expect_error(charts.PerformanceSummary(R_q), NA)
  
  # yearly
  y_idx <- as.Date(paste0(2020:2025, "-01-01"))
  R_y <- xts(rnorm(6, 0, 0.01), y_idx)
  expect_error(charts.PerformanceSummary(R_y), NA)
})
