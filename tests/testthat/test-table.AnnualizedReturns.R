library(PerformanceAnalytics)
library(xts)

test_that("table.AnnualizedReturns covers multiple frequency scaling states", {
  skip_on_cran()
  
  # Minute
  m_idx <- as.POSIXct("2020-01-01 09:30:00") + 60 * 1:10
  R_m <- xts(rnorm(10, 0, 0.01), m_idx)
  expect_error(table.AnnualizedReturns(R_m), "Data periodicity too high")
  
  # Hourly
  h_idx <- as.POSIXct("2020-01-01 09:00:00") + 3600 * 1:10
  R_h <- xts(rnorm(10, 0, 0.01), h_idx)
  expect_error(table.AnnualizedReturns(R_h), "Data periodicity too high")
  
  # Daily
  d_idx <- as.Date("2020-01-01") + 1:10
  R_d <- xts(rnorm(10, 0, 0.01), d_idx)
  res_d <- table.AnnualizedReturns(R_d)
  expect_equal(dim(res_d)[1], 3)
  
  # Weekly
  w_idx <- as.Date("2020-01-01") + 7 * 1:10
  R_w <- xts(rnorm(10, 0, 0.01), w_idx)
  res_w <- table.AnnualizedReturns(R_w)
  expect_equal(dim(res_w)[1], 3)
  
  # Quarterly
  q_idx <- as.Date(paste0("2020-0", c(1,4,7), "-01"))
  R_q <- xts(rnorm(4, 0, 0.01), c(q_idx, as.Date("2020-10-01")))
  res_q <- table.AnnualizedReturns(R_q)
  expect_equal(dim(res_q)[1], 3)
  
  # Yearly
  y_idx <- as.Date(paste0(2020:2025, "-01-01"))
  R_y <- xts(rnorm(6, 0, 0.01), y_idx)
  res_y <- table.AnnualizedReturns(R_y)
  expect_equal(dim(res_y)[1], 3)
  
  # Multivariate inputs and alternate geometric chain
  R_multi <- cbind(R_d, R_d * 2)
  res_multi <- table.AnnualizedReturns(R_multi, geometric=FALSE)
  expect_equal(dim(res_multi)[2], 2)
  
  # Rf Time series vector input
  Rf_vec <- xts(rep(0.01, 10), d_idx)
  res_rf <- table.AnnualizedReturns(R_multi, Rf = Rf_vec)
  expect_equal(dim(res_rf)[2], 2)
})

test_that("table.InformationRatio covers multiple frequency scaling states", {
  skip_on_cran()
  
  # Hourly
  h_idx <- as.POSIXct("2020-01-01 09:00:00") + 3600 * 1:10
  R_h <- xts(rnorm(10, 0, 0.01), h_idx)
  Rb_h <- R_h
  expect_error(table.InformationRatio(R_h, Rb_h), "Data periodicity too high")
  
  # Daily
  d_idx <- as.Date("2020-01-01") + 1:10
  R_d <- xts(rnorm(10, 0, 0.01), d_idx)
  Rb_d <- xts(rnorm(10, 0, 0.01), d_idx)
  res_d <- table.InformationRatio(R_d, Rb_d)
  expect_equal(dim(res_d)[1], 3)
  
  # Weekly
  w_idx <- as.Date("2020-01-01") + 7 * 1:10
  R_w <- xts(rnorm(10, 0, 0.01), w_idx)
  Rb_w <- xts(rnorm(10, 0, 0.01), w_idx)
  res_w <- table.InformationRatio(R_w, Rb_w)
  expect_equal(dim(res_w)[1], 3)
  
  # Quarterly
  q_idx <- as.Date(paste0("2020-0", c(1,4,7), "-01"))
  R_q <- xts(rnorm(4, 0, 0.01), c(q_idx, as.Date("2020-10-01")))
  Rb_q <- xts(rnorm(4, 0, 0.01), c(q_idx, as.Date("2020-10-01")))
  res_q <- table.InformationRatio(R_q, Rb_q)
  expect_equal(dim(res_q)[1], 3)
  
  # Yearly
  y_idx <- as.Date(paste0(2020:2025, "-01-01"))
  R_y <- xts(rnorm(6, 0, 0.01), y_idx)
  Rb_y <- xts(rnorm(6, 0, 0.01), y_idx)
  res_y <- table.InformationRatio(R_y, Rb_y)
  expect_equal(dim(res_y)[1], 3)
  
  # Multivariate matrix inputs
  R_multi <- cbind(R_d, R_d * 2)
  res_multi <- table.InformationRatio(R_multi, Rb_d)
  expect_equal(dim(res_multi)[2], 2)
})

test_that("table.DrawdownsRatio covers multiple frequency scaling states", {
  skip_on_cran()
  
  # Hourly
  h_idx <- as.POSIXct("2020-01-01 09:00:00") + 3600 * 1:10
  R_h <- xts(rnorm(10, 0, 0.01), h_idx)
  expect_error(table.DrawdownsRatio(R_h), "Data periodicity too high")
  
  # Daily
  d_idx <- as.Date("2020-01-01") + 1:10
  R_d <- xts(rnorm(10, 0, 0.01), d_idx)
  res_d <- table.DrawdownsRatio(R_d)
  expect_equal(dim(res_d)[1], 7)
  
  # Weekly
  w_idx <- as.Date("2020-01-01") + 7 * 1:10
  R_w <- xts(rnorm(10, 0, 0.01), w_idx)
  res_w <- table.DrawdownsRatio(R_w)
  expect_equal(dim(res_w)[1], 7)
  
  # Quarterly
  q_idx <- as.Date(paste0("2020-0", c(1,4,7), "-01"))
  R_q <- xts(rnorm(4, 0, 0.01), c(q_idx, as.Date("2020-10-01")))
  res_q <- table.DrawdownsRatio(R_q)
  expect_equal(dim(res_q)[1], 7)
  
  # Yearly
  y_idx <- as.Date(paste0(2020:2025, "-01-01"))
  R_y <- xts(rnorm(6, 0, 0.01), y_idx)
  res_y <- table.DrawdownsRatio(R_y)
  expect_equal(dim(res_y)[1], 7)
  
  # Multivariate matrix inputs
  R_multi <- cbind(R_d, R_d * 2)
  res_multi <- table.DrawdownsRatio(R_multi)
  expect_equal(dim(res_multi)[2], 2)
})
