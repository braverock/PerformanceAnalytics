library(PerformanceAnalytics)
library(vdiffr)

test_that("chart.Correlation scatterplot panels dynamically reflect Pearson and Spearman methods (fixes #105)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(123)
  x <- rnorm(100)
  y <- x^3
  df <- data.frame(x = x, y = y)

  pdf(file = NULL)

  # A. Pearson (Default) -> Should visibly display the cubic x^3 curve
  p_pearson <- function() chart.Correlation(df, method = "pearson")
  vdiffr::expect_doppelganger("chart-Correlation-pearson", p_pearson)

  # B. Spearman -> Should evaluate ranks, resulting in a perfectly linear scatterplot
  p_spearman <- function() chart.Correlation(df, method = "spearman")
  vdiffr::expect_doppelganger("chart-Correlation-spearman", p_spearman)

  dev.off()
})

test_that("chart.Correlation correctly routes pch to the underlying scatterplot panels (fixes #87)", {
  skip_on_cran()

  data(edhec)
  x <- edhec[1:50, 1:3]

  pdf(file = NULL)

  # A. Test string-based plotting character
  p_pch_char <- function() chart.Correlation(x, pch = "+")
  vdiffr::expect_doppelganger("chart-Correlation-pch-char", p_pch_char)

  # B. Test numeric-based plotting character
  p_pch_num <- function() chart.Correlation(x, pch = 19)
  vdiffr::expect_doppelganger("chart-Correlation-pch-num", p_pch_num)
})
