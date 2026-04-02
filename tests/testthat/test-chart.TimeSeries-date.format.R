library(PerformanceAnalytics)
library(vdiffr)

test_that("chart.TimeSeries date.format is correctly respected by built-in and ggplot2 engines", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggplot2")
  
  data(edhec)
  # Take a small subset to ensure sparse date rendering focuses on our format
  R <- edhec[1:24, 1, drop=FALSE]
  
  pdf(file=NULL)
  
  # 1. Base engine with custom date format
  p1 <- chart.TimeSeries(R, plot.engine="default", date.format="%b-%Y")
  vdiffr::expect_doppelganger("chart-TimeSeries-base-date-format", p1)
  
  # 2. ggplot2 engine with custom date format
  p2 <- chart.TimeSeries(R, plot.engine="ggplot2", date.format="%b-%Y")
  vdiffr::expect_doppelganger("chart-TimeSeries-ggplot2-date-format", p2)
  
  dev.off()
})
