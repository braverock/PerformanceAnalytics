library(PerformanceAnalytics)
library(vdiffr)

test_that("chart.CumReturns accurately translates ylog=TRUE to all underlying engines (fixes #91)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  # Build deterministic data showing an exponential curve
  # E.g., steady 5% returns per period compounds exponentially
  dates <- as.Date("2000-01-01") + c(1:100)
  R_exp <- xts::xts(matrix(rep(0.05, 100), ncol=1), order.by=dates)
  colnames(R_exp) <- "Asset"
  
  pdf(file=NULL)
  
  # A. Base engine (plot.xts) 
  p_base_linear <- function() chart.CumReturns(R_exp, plot.engine="default", ylog=FALSE)
  p_base_log    <- function() chart.CumReturns(R_exp, plot.engine="default", ylog=TRUE)
  
  vdiffr::expect_doppelganger("chart-CumReturns-base-linear", p_base_linear)
  vdiffr::expect_doppelganger("chart-CumReturns-base-log", p_base_log)
  
  # B. ggplot2 engine
  if (requireNamespace("ggplot2", quietly=TRUE)) {
      p_gg_linear <- function() chart.CumReturns(R_exp, plot.engine="ggplot2", ylog=FALSE)
      p_gg_log    <- function() chart.CumReturns(R_exp, plot.engine="ggplot2", ylog=TRUE)
      
      vdiffr::expect_doppelganger("chart-CumReturns-ggplot2-linear", p_gg_linear)
      vdiffr::expect_doppelganger("chart-CumReturns-ggplot2-log", p_gg_log)
  }
  
  dev.off()
})
