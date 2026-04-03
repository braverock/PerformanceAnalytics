library(PerformanceAnalytics)
library(xts)

test_that("Refactored charting functions are silent when called with default args (fixes #35)", {
  skip_on_cran()
  data(managers)
  R <- managers[, 1:2]
  
  pdf(file=NULL)
  on.exit(dev.off())
  
  # These should be silent (no deprecation warnings)
  expect_silent(charts.TimeSeries(R))
  expect_silent(charts.Bar(R))
  
  # These should still warn if the deprecated argument is explicitly provided
  expect_warning(charts.TimeSeries(R, space=1), "The space argument of chart.TimeSeries has been deprecated")
  expect_warning(charts.Bar(R, cex.legend=1), "The cex.legend argument of chart.TimeSeries has been deprecated")
})
