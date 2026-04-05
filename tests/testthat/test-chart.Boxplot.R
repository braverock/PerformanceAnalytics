library(PerformanceAnalytics)
library(xts)

test_that("chart.Boxplot evaluates sorting options and engines correctly", {
  skip_on_cran()
  data(managers)
  R <- managers[, 1:4]
  
  pdf(file = NULL)
  dev.control(displaylist = "enable")
  on.exit(dev.off())
  
  # A. Base R Graphics Options
  expect_s3_class(chart.Boxplot(R), "recordedplot")
  expect_s3_class(chart.Boxplot(R, as.Tufte = TRUE), "recordedplot")
  
  # B. Sorting Logic
  expect_s3_class(chart.Boxplot(R, sort.by = "mean"), "recordedplot")
  expect_s3_class(chart.Boxplot(R, sort.by = "median"), "recordedplot")
  expect_s3_class(chart.Boxplot(R, sort.by = "variance"), "recordedplot")
  
  expect_s3_class(chart.Boxplot(R, sort.by = "mean", sort.ascending = TRUE), "recordedplot")
  
  # C. Show Data (overlay numeric observations)
  expect_s3_class(chart.Boxplot(R, show.data = c(1, 2)), "recordedplot")
  
  # D. Modern Plot Engines
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_s3_class(chart.Boxplot(R, plot.engine = "ggplot2"), "ggplot")
    expect_s3_class(chart.Boxplot(R, plot.engine = "ggplot2", sort.by = "mean"), "ggplot")
  }
  
  if (requireNamespace("plotly", quietly = TRUE)) {
    expect_s3_class(chart.Boxplot(R, plot.engine = "plotly"), "plotly")
    expect_s3_class(chart.Boxplot(R, plot.engine = "plotly", sort.by = "variance"), "plotly")
  }
  
  # E. Fallback and Exceptions
  expect_warning(res_bad <- chart.Boxplot(R, plot.engine = "invalid"), "Ploting chart using built-in engine now")
  expect_s3_class(res_bad, "recordedplot")
})
