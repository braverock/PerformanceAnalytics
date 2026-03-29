library(PerformanceAnalytics)

test_that("Charting functions work", {
  skip_on_cran()
  data(edhec)
  data(managers)
  
  R <- edhec[1:50, 1:4]
  R_mgr <- managers[1:50, 1:4]
  
  # chart.BarVaR
  # Suppress warnings and plots to keep logs clean
  pdf(file = NULL)
  
  expect_error(chart.BarVaR(R, methods=c("HistoricalVaR", "ModifiedVaR")), NA)
  
  # charts.PerformanceSummary
  expect_error(charts.PerformanceSummary(R), NA)
  
  # chart.Histogram
  expect_error(chart.Histogram(R[,1]), NA)
  
  # chart.SnailTrail
  expect_error(chart.SnailTrail(R_mgr, Rf=0.03/12), NA)
  
  # chart.QQPlot
  expect_error(chart.QQPlot(R[,1]), NA)
  
  dev.off()
})

test_that("Table functions work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  # table.Stats
  res_stats <- table.Stats(R)
  expect_true(is.data.frame(res_stats) || is.matrix(res_stats))
  
  # table.DownsideRisk
  res_dr <- table.DownsideRisk(R)
  expect_true(is.data.frame(res_dr) || is.matrix(res_dr))
})


test_that("table.SFM and textplot work", {
  skip_on_cran()
  data(managers)
  
  # table.SFM
  res_sfm <- table.SFM(managers[,1:3], managers[,8], Rf = managers[,10])
  expect_true(is.data.frame(res_sfm))
  
  # textplot.matrix
  pdf(file = NULL)
  expect_error(textplot(res_sfm), NA)
  expect_error(textplot(matrix(rnorm(20), ncol=4)), NA)
  dev.off()
})

test_that("PerformanceAnalytics::legend works", {
  skip_on_cran()
  
  pdf(file = NULL)
  plot(1:10)
  expect_error(PerformanceAnalytics::legend("topright", legend=c("A", "B")), NA)
  dev.off()
})

test_that("charts.BarVaR works", {
  skip_on_cran()
  data(managers)
  
  pdf(file = NULL)
  expect_error(charts.BarVaR(managers[,1:3]), NA)
  dev.off()
})

test_that("chart.TimeSeries engines work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  pdf(file = NULL)
  
  # default engine invokes chart.TimeSeries.base
  expect_error(chart.TimeSeries(R, plot.engine="default"), NA)
  
  dev.off()
})

test_that("textplot.character works", {
  skip_on_cran()
  
  pdf(file = NULL)
  expect_error(textplot("Hello World", halign="center", valign="center"), NA)
  expect_error(textplot(c("Multi", "Line", "String")), NA)
  dev.off()
})
