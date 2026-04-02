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
