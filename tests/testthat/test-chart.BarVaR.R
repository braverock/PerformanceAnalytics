library(PerformanceAnalytics)
library(xts)

test_that("chart.BarVaR evaluates all risk methods and bounds successfully", {
  skip_on_cran()
  data(managers)
  R <- managers[, 1, drop=FALSE]
  
  pdf(file = NULL)
  dev.control(displaylist = "enable")
  on.exit(dev.off())
  
  methods <- c("ModifiedVaR", "GaussianVaR", "HistoricalVaR", "StdDev", 
               "ModifiedES", "GaussianES", "HistoricalES")
  
  for (m in methods) {
      expect_s3_class(chart.BarVaR(R, methods=m), "replot_xts")
  }
  
  # Multiple combinations
  expect_s3_class(chart.BarVaR(R, methods=c("GaussianVaR", "HistoricalVaR")), "replot_xts")
  
  # Boolean visualization flags
  expect_s3_class(chart.BarVaR(R, show.clean=TRUE), "replot_xts")
  expect_s3_class(chart.BarVaR(R, show.horizontal=TRUE), "replot_xts")
  expect_s3_class(chart.BarVaR(R, show.symmetric=TRUE), "replot_xts")
  expect_s3_class(chart.BarVaR(R, show.endvalue=TRUE, show.greenredbars=TRUE), "replot_xts")
})

test_that("chart.BarVaR evaluates all risk methods and correctly passes explicit moments", {
  skip_on_cran()
  data(managers)
  R <- managers[, 1, drop=FALSE]
  
  pdf(file = NULL)
  dev.control(displaylist = "enable")
  on.exit(dev.off())
  
  # Ensure no error when passing explicit moments and other valid VaR arguments
  # along with standard plot parameters like lwd
  expect_error(
    chart.BarVaR(R, methods=c("ModifiedVaR", "ModifiedES"), 
                 mu=0.05, sigma=0.10, m3=-1.5, m4=8.0, 
                 invert=FALSE, lwd=2), 
    NA)
})
