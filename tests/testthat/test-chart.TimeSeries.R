library(PerformanceAnalytics)
library(vdiffr)

test_that("chart.TimeSeries supports type='bar' natively across engines", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggplot2")

  data(edhec)
  R_uni <- edhec[1:50, 1, drop = FALSE]
  R_multi <- edhec[1:50, 1:2]

  pdf(file = NULL)

  # 1. Base engine with line (default)
  p1 <- chart.TimeSeries(R_uni, plot.engine = "default")
  vdiffr::expect_doppelganger("chart-TimeSeries-base-line", p1)

  # 2. Base engine with bar type should not throw warnings
  p2 <- expect_warning(chart.TimeSeries(R_uni, plot.engine = "default", type = "bar"), NA)
  vdiffr::expect_doppelganger("chart-TimeSeries-base-bar", p2)

  # 3. ggplot2 engine with bar type handles multi-column dodge
  p3 <- expect_warning(chart.TimeSeries(R_multi, plot.engine = "ggplot2", type = "bar"), NA)
  vdiffr::expect_doppelganger("chart-TimeSeries-ggplot2-bar", p3)

  dev.off()
})

test_that("chart.TimeSeries mathematically enforces ylog boundary rules and issues warnings on impossible distributions", {
  skip_on_cran()
  
  data(edhec)
  # Edhec inherently contains negative returns!
  R_neg <- edhec[1:50, 1, drop=FALSE]
  
  # 1. Expect warning on negative data when ylog=TRUE
  pdf(file=NULL)
  expect_warning(chart.TimeSeries(R_neg, ylog=TRUE), "data contains zero or negative values")
  
  # 2. Extract an absolutely positive wealth-index structure from the exact same dataset
  # Cumulating returns to a wealth index inherently strictly bounds the plot space strictly > 0.
  R_pos <- cumprod(1 + R_neg)
  
  # Expect NO warnings, as all values > 0 are mathematically valid under log10 translation
  expect_warning(chart.TimeSeries(R_pos, ylog=TRUE), NA)
  
  dev.off()
})
