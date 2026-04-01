library(PerformanceAnalytics)
test_that("UpsidePotentialRatio handles MAR as scalar and xts properly", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1]
  
  # (a) scalar MAR
  res_scalar <- UpsidePotentialRatio(R, MAR = 0)
  expect_true(is.numeric(res_scalar))
  
  # (b) xts MAR vector same length
  MAR_xts <- R * 0 + 0.01
  res_xts <- UpsidePotentialRatio(R, MAR = MAR_xts)
  expect_true(is.numeric(res_xts))
  
  # Check if numeric MAR vector same length works
  MAR_vec <- rep(0.01, 50)
  res_vec <- UpsidePotentialRatio(R, MAR = MAR_vec)
  expect_equal(as.numeric(res_xts), as.numeric(res_vec))
  
  # (c) mismatched-length MAR raises informative error
  MAR_bad <- rep(0.01, 10)
  expect_error(UpsidePotentialRatio(R, MAR = MAR_bad), "Rf|MAR.*length|length of Rf")
})
