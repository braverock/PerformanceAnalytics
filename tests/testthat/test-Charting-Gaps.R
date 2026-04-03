library(PerformanceAnalytics)
library(vdiffr)

test_that("chart.ACF and chart.ACFplus render correctly (fixes coverage gap)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  data(edhec)
  R <- edhec[1:100, 1, drop=FALSE]
  
  pdf(file=NULL)
  on.exit(dev.off())
  
  # chart.ACF
  p_acf <- function() chart.ACF(R)
  vdiffr::expect_doppelganger("chart-ACF", p_acf)
  
  # chart.ACFplus
  p_acfplus <- function() chart.ACFplus(R)
  vdiffr::expect_doppelganger("chart-ACFplus", p_acfplus)
})

test_that("chart.Scatter renders correctly (fixes coverage gap)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  data(edhec)
  x <- edhec[1:100, 1]
  y <- edhec[1:100, 2]
  
  pdf(file=NULL)
  on.exit(dev.off())
  
  p_scatter <- function() chart.Scatter(x, y)
  vdiffr::expect_doppelganger("chart-Scatter", p_scatter)
})

test_that("chart.Events renders correctly (fixes coverage gap)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  data(managers)
  R <- Drawdowns(managers[, 2, drop=FALSE])
  n <- table.Drawdowns(managers[, 2, drop=FALSE])
  
  pdf(file=NULL)
  on.exit(dev.off())
  
  p_events <- function() chart.Events(R, 
                                     dates = n$Trough, 
                                     prior = 6, 
                                     post = 6, 
                                     main = "Worst Drawdowns")
  vdiffr::expect_doppelganger("chart-Events", p_events)
})
