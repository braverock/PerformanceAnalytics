library(PerformanceAnalytics)
library(xts)

test_that("chart.StackedBar evaluates xts vs matrix branches", {
  skip_on_cran()
  
  # Set up data
  data(managers)
  w_xts <- managers[1:5, 1:3]
  w_mat <- as.matrix(w_xts)
  
  pdf(file=NULL)
  dev.control(displaylist="enable")
  on.exit(dev.off())
  
  # A. xts path
  expect_s3_class(chart.StackedBar(w_xts), "recordedplot")
  
  # B. matrix path
  expect_s3_class(chart.StackedBar(w_mat), "recordedplot")
  
  # C. 1-row matrix (forces chart.StackedBar.matrix)
  expect_s3_class(chart.StackedBar(w_xts[1, , drop=FALSE]), "recordedplot")
  
  # D. Edge case rendering configurations
  expect_s3_class(chart.StackedBar(w_xts, unstacked=FALSE, legend.loc="topleft"), "recordedplot")
  expect_s3_class(chart.StackedBar(w_xts, unstacked=TRUE, xaxis=FALSE, legend.loc=NULL), "recordedplot")
  expect_s3_class(chart.StackedBar(w_mat, unstacked=FALSE, legend.loc="topleft"), "recordedplot")
})
