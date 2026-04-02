library(PerformanceAnalytics)
library(xts)
library(zoo)

test_that("chart.RiskReturnScatter bounds entirely NA columns without plotting limits crashes (fixes #107)", {
  skip_on_cran()
  
  pdf(file=NULL)
  
  # A. Entirely NA vectors evaluate without plot limit crashes (fixes #107 primary bug)
  x_na <- data.frame(
    "HR Muni Bond" = rep(NA_real_, 4),
    "Composite Portfolio" = c(-0.0025, -0.0012, 0.0028, -0.0004),
    check.names=FALSE
  )
  rownames(x_na) <- c("2017-12-31", "2018-03-31", "2018-06-30", "2018-09-30")
  expect_error(suppressWarnings(chart.RiskReturnScatter(x_na, scale=4)), NA)
  
  # B. Test na.fill=0 explicitly fixes the empty asset variance bounds natively
  expect_error(chart.RiskReturnScatter(x_na, na.fill=0, scale=4), NA)
  
  dev.off()
})

test_that("chart.RiskReturnScatter properly rejects data without valid time indices", {
  skip_on_cran()
  
  pdf(file=NULL)
  
  # The user accidentally subsets one column without drop=FALSE, turning it into a numeric vector
  # without POSIXct dates, which causes checkData(method="matrix") to construct a matrix 
  # with integer rownames resulting in `as.POSIXct` crashes inside table.AnnualizedReturns.
  x_vec <- c(-0.0025, -0.0012, 0.0028, -0.0004)
  
  # This explicitly proves the system rejects broken integer-indexed time series
  expect_error(suppressWarnings(chart.RiskReturnScatter(x_vec, scale=4)), "The data cannot be converted into a time series")
  
  # 2D zoo object with integer indices (lacks explicit dates)
  x_zoo <- zoo::zoo(matrix(c(-0.0025, -0.0012, 0.0028, -0.0004), ncol=1), 1:4)
  expect_error(suppressWarnings(chart.RiskReturnScatter(x_zoo, scale=4)), "The data cannot be converted into a time series")
  
  # Base R matrix with non-date rownames (e.g. 1, 2, 3, 4)
  x_mat <- matrix(c(-0.0025, -0.0012, 0.0028, -0.0004), ncol=1)
  rownames(x_mat) <- c("1", "2", "3", "4")
  expect_error(suppressWarnings(chart.RiskReturnScatter(x_mat, scale=4)), "The data cannot be converted into a time series")
  
  dev.off()
})
