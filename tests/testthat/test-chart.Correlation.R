library(PerformanceAnalytics)
library(vdiffr)

test_that("chart.Correlation scatterplot panels dynamically reflect Pearson and Spearman methods (fixes #105)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  set.seed(123)
  x <- rnorm(100)
  y <- x^3 
  df <- data.frame(x=x, y=y)
  
  pdf(file=NULL)
  
  # A. Pearson (Default) -> Should visibly display the cubic x^3 curve
  p_pearson <- function() chart.Correlation(df, method="pearson")
  vdiffr::expect_doppelganger("chart-Correlation-pearson", p_pearson)
  
  # B. Spearman -> Should evaluate ranks, resulting in a perfectly linear scatterplot
  p_spearman <- function() chart.Correlation(df, method="spearman")
  vdiffr::expect_doppelganger("chart-Correlation-spearman", p_spearman)
  
  dev.off()
})
