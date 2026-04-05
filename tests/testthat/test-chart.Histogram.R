library(PerformanceAnalytics)
library(xts)

test_that("chart.Histogram covers all visualization overlays", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1]

  pdf(file = NULL)
  dev.control(displaylist = "enable")
  on.exit(dev.off())

  # A. Base Histogram
  expect_s3_class(chart.Histogram(R), "recordedplot")

  # B. Statistical Methods Overlays
  expect_s3_class(chart.Histogram(R, methods = "add.normal"), "recordedplot")
  expect_s3_class(chart.Histogram(R, methods = "add.density"), "recordedplot")
  expect_s3_class(suppressWarnings(chart.Histogram(R, methods = "add.cauchy")), "recordedplot")
  expect_s3_class(chart.Histogram(R, methods = "add.rug"), "recordedplot")
  expect_s3_class(chart.Histogram(R, methods = "add.centered"), "recordedplot")

  # C. Combinations
  expect_s3_class(chart.Histogram(R, methods = c("add.normal", "add.density", "add.rug")), "recordedplot")
  expect_s3_class(suppressWarnings(chart.Histogram(R, methods = c("add.cauchy", "add.centered"))), "recordedplot")

  # D. Additional Overlays
  expect_s3_class(chart.Histogram(R, show.outliers = TRUE), "recordedplot")

  # E. Test with risk metrics overlaid
  # Note: Modified VaR/ES overlays require methods array to contain 'add.risk'
  expect_s3_class(chart.Histogram(R, methods = "add.risk", p = 0.95), "recordedplot")
})
