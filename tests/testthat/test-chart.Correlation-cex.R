library(PerformanceAnalytics)
library(vdiffr)

test_that("chart.Correlation correctly evaluates cex.cor arguments to scale text (fixes #99)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  data(edhec)
  x <- edhec[1:50, 1:3]
  
  pdf(file=NULL)
  
  # Base chart (default dynamic sizing)
  p_base <- function() chart.Correlation(x)
  vdiffr::expect_doppelganger("chart-Correlation-cex-default", p_base)
  
  # Large custom cex.cor sizes
  # This previously crashed immediately with 'object cex not found'
  p_large <- function() chart.Correlation(x, cex.cor = 3)
  
  expect_error(chart.Correlation(x, cex.cor = 3), NA)
  vdiffr::expect_doppelganger("chart-Correlation-cex-large", p_large)
  
  dev.off()
})
