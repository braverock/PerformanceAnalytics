library(PerformanceAnalytics)

test_that("DownsideDeviation handles MAR vector lengths correctly", {
  skip_on_cran()

  data(edhec)
  R <- edhec[1:50, 1]

  # (a) scalar MAR
  res_scalar <- DownsideDeviation(R, MAR = 0)
  expect_true(is.numeric(res_scalar))

  # (b) xts MAR vector same length
  MAR_xts <- R * 0 + 0.01 # xts vector of 0.01
  res_xts <- DownsideDeviation(R, MAR = MAR_xts)
  expect_true(is.numeric(res_xts))

  # Check if numeric MAR vector same length works
  MAR_vec <- rep(0.01, 50)
  res_vec <- DownsideDeviation(R, MAR = MAR_vec)
  expect_equal(as.numeric(res_xts), as.numeric(res_vec))

  # (c) mismatched-length MAR raises informative error
  MAR_bad <- rep(0.01, 10)
  expect_error(DownsideDeviation(R, MAR = MAR_bad), "MAR.*length|length.*MAR|subset")
})

test_that("SortinoRatio works with grouped tibbles", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")
  
  library(dplyr)
  library(tibble)
  
  df <- tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    ret1 = c(0.01, -0.02, 0.03, -0.01),
    ret2 = c(0.02, -0.01, 0.01, -0.03)
  )
  df_grouped <- df %>% group_by(date)
  
  # Ensure it doesn't fail with the internal checkData usage
  res <- SortinoRatio(df_grouped, MAR = 0)
  expect_true(is.matrix(res) || is.data.frame(res) || is.numeric(res))
})
