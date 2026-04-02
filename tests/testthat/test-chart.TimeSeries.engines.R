library(PerformanceAnalytics)
test_that("chart.TimeSeries supports bar type without crashing on all engines", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1, drop=FALSE]
  
  # plotly
  if (requireNamespace("plotly", quietly=TRUE)) {
    p_plotly <- chart.TimeSeries(R, plot.engine="plotly", type="bar")
    expect_true(inherits(p_plotly, "plotly"))
  }
  
  # dygraphs
  if (requireNamespace("dygraphs", quietly=TRUE)) {
    p_dygraph <- chart.TimeSeries(R, plot.engine="dygraph", type="bar")
    expect_true(inherits(p_dygraph, "dygraphs"))
  }
  
  # googlevis (returns a gvis object)
  if (requireNamespace("googleVis", quietly=TRUE)) {
    p_googlevis <- chart.TimeSeries(R, plot.engine="googlevis", type="bar")
    # Googlevis automatically calls plot(p_g) if returned!
    # Wait, chart.TimeSeries.base actually returns NULL because it does: `plot(p_g)` which returns NULL.
    # We just expect no error!
    expect_error(suppressWarnings(chart.TimeSeries(R, plot.engine="googlevis", type="bar")), NA)
  }
})
