library(PerformanceAnalytics)
library(xts)

test_that("Base R graphics charts are returned as objects (fixes #171)", {
  skip_on_cran()
  data(managers)
  R <- managers[,1, drop=FALSE]
  Rb <- managers[,8, drop=FALSE]
  
  pdf(file=NULL)
  dev.control(displaylist="enable")
  on.exit(dev.off())
  
  # Group C: Functions that previously returned NULL
  expect_s3_class(chart.ACF(R), "recordedplot")
  expect_s3_class(chart.Boxplot(R), "recordedplot")
  expect_s3_class(chart.CaptureRatios(R, Rb), "recordedplot")
  expect_s3_class(chart.Correlation(managers[,1:3]), "recordedplot")
  expect_s3_class(chart.ECDF(R), "recordedplot")
  expect_s3_class(chart.Histogram(R), "recordedplot")
  expect_s3_class(chart.QQPlot(R), "recordedplot")
  expect_s3_class(chart.RiskReturnScatter(managers[,1:3]), "recordedplot")
  expect_s3_class(chart.Scatter(R, Rb), "recordedplot")
  expect_s3_class(chart.SnailTrail(R), "recordedplot")
  expect_s3_class(chart.VaRSensitivity(R), "recordedplot")
  expect_s3_class(charts.BarVaR(R), "recordedplot")
  
  # Group B: Functions that previously returned side-effect lists like par()
  expect_s3_class(chart.ACFplus(R), "recordedplot")
  expect_s3_class(chart.StackedBar(R), "recordedplot")
  expect_s3_class(charts.PerformanceSummary(R), "recordedplot")
  expect_s3_class(charts.RollingPerformance(R), "recordedplot")
})
