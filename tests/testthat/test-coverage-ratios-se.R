library(PerformanceAnalytics)

test_that("SE computation works in Deviation and Ratio functions", {
  skip_on_cran()
  
  if (requireNamespace("RPESE", quietly = TRUE)) {
    data(edhec)
    # Using a single column, as SE often requires single portfolio methods
    R <- edhec[1:50, 1, drop=FALSE]
    
    # DownsideDeviation
    res_dd <- DownsideDeviation(R, MAR=0, method="full", SE=TRUE)
    expect_true(is.matrix(res_dd) && nrow(res_dd) > 1)
    
    # SemiDeviation
    res_sd <- SemiDeviation(R, SE=TRUE)
    expect_true(is.matrix(res_sd) && nrow(res_sd) > 1)
    
    # StdDev
    res_std <- StdDev(R, clean="none", portfolio_method="single", SE=TRUE)
    expect_true(is.matrix(res_std) && nrow(res_std) > 1)
    
    # SemiSD
    res_smd <- SemiSD(R, SE=TRUE)
    expect_true(is.matrix(res_smd) && nrow(res_smd) > 1)
    
    # SortinoRatio
    res_sor <- SortinoRatio(R, MAR=0, SE=TRUE)
    expect_true(is.matrix(res_sor) && nrow(res_sor) > 1)
    
    # SortinoRatio with weights
    res_sor_w <- SortinoRatio(edhec[1:50, 1:2], MAR=0, weights=c(0.5, 0.5))
    expect_true(is.numeric(res_sor_w))
    
    # DownsideSharpeRatio
    res_dsr <- DownsideSharpeRatio(R, MAR=0, SE=TRUE)
    expect_true(is.matrix(res_dsr) && nrow(res_dsr) > 1)
    
    # Omega
    res_om <- Omega(R, L=0, method="simple", output="point", SE=TRUE)
    expect_true(is.matrix(res_om) && nrow(res_om) > 1)
  }
})

test_that("CalmarRatio and SterlingRatio with different periodicities", {
  skip_on_cran()
  
  # create daily, weekly, quarterly, yearly xts
  dts_daily <- seq(as.Date("2020-01-01"), by="day", length.out=500)
  x_daily <- xts(rnorm(500)/100, order.by=dts_daily)
  expect_true(is.matrix(CalmarRatio(x_daily)))
  expect_true(is.matrix(SterlingRatio(x_daily)))
  
  dts_weekly <- seq(as.Date("2020-01-01"), by="week", length.out=100)
  x_weekly <- xts(rnorm(100)/100, order.by=dts_weekly)
  expect_true(is.matrix(CalmarRatio(x_weekly)))
  
  dts_quarterly <- seq(as.Date("2020-01-01"), by="quarter", length.out=40)
  x_quarterly <- xts(rnorm(40)/100, order.by=dts_quarterly)
  expect_true(is.matrix(CalmarRatio(x_quarterly)))
  
  dts_yearly <- seq(as.Date("2020-01-01"), by="year", length.out=10)
  x_yearly <- xts(rnorm(10)/100, order.by=dts_yearly)
  expect_true(is.matrix(CalmarRatio(x_yearly)))
})

test_that("DownsideDeviation specific branches", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  # MAR as time series
  mar_ts <- R[,1] / 10 # dummy benchmark
  res_mar_ts <- DownsideDeviation(R[,2], MAR=mar_ts)
  expect_true(is.numeric(res_mar_ts) || is.matrix(res_mar_ts))
  
  # MAR as conformable matrix (so R < MAR succeeds) but not timeBased(index)
  mar_mat <- matrix(0.01, 50, 1)
  res_mar_mat <- DownsideDeviation(R[,2], MAR=mar_mat)
  expect_true(is.numeric(res_mar_mat) || is.matrix(res_mar_mat))
  
  # potential=TRUE across matrix
  res_pot <- DownsideDeviation(R, MAR=0, potential=TRUE)
  expect_true(is.matrix(res_pot))
  
  # SE=TRUE with potential=TRUE
  expect_warning(res_pot_se <- DownsideDeviation(R[,2,drop=FALSE], method="full", potential=TRUE, SE=TRUE), 
                 "To return SEs, \"potential\" must be FALSE")
  expect_true(is.matrix(res_pot_se))
})

test_that("CalmarRatio and SterlingRatio minute/hourly error checks", {
  skip_on_cran()
  
  dts_minute <- seq(as.POSIXct("2020-01-01 00:00:00"), by="min", length.out=10)
  x_minute <- xts(rnorm(10)/100, order.by=dts_minute)
  
  expect_error(CalmarRatio(x_minute), "Data periodicity too high")
  expect_error(SterlingRatio(x_minute), "Data periodicity too high")
  
  dts_hourly <- seq(as.POSIXct("2020-01-01 00:00:00"), by="hour", length.out=10)
  x_hourly <- xts(rnorm(10)/100, order.by=dts_hourly)
  
  expect_error(CalmarRatio(x_hourly), "Data periodicity too high")
  expect_error(SterlingRatio(x_hourly), "Data periodicity too high")
})

test_that("SE=TRUE with invalid arguments tests", {
  skip_on_cran()
  
  if (requireNamespace("RPESE", quietly = TRUE)) {
    data(edhec)
    R <- edhec[1:50, 1, drop=FALSE]
    
    # DownsideDeviation invalid method for SE
    expect_warning(res <- DownsideDeviation(R, MAR=0, method="subset", SE=TRUE),
                   "To return SEs, \"method\" must be \"full\"")
    expect_true(is.matrix(res) && nrow(res) > 1)
    
    # Omega invalid method and output for SE
    expect_warning(res <- Omega(R, L=0, method="interp", output="point", SE=TRUE),
                   "To return SEs, \"method\" must be \"simple\"")
    expect_true(is.matrix(res) && nrow(res) > 1)
  }
})

test_that("SemiSD and Omega unreached branches", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1, drop=TRUE] # vector
  
  expect_true(is.numeric(SemiSD(R)))
  expect_true(is.numeric(SemiDeviation(R)))
  
  R_mat <- edhec[1:50, 1:2]
  expect_error(suppressWarnings(Omega(R_mat, method="binomial")))
  expect_error(suppressWarnings(Omega(R_mat, method="blackscholes")))
  
  # length L > 1 and length Rf > 1
  L_vec <- c(0.01, 0.02)
  Rf_vec <- c(0.001, 0.002)
  res3 <- Omega(R_mat, L=L_vec, Rf=Rf_vec)
  expect_true(is.matrix(res3))
})

test_that("SE invalid arguments warnings for StdDev, SortinoRatio, etc.", {
  skip_on_cran()
  
  if (requireNamespace("RPESE", quietly = TRUE)) {
    data(edhec)
    R <- edhec[1:50, 1:2]
    
    # StdDev SE warnings
    expect_warning(StdDev(R[,1], clean="boudt", SE=TRUE), 
                   "To return SEs, \"clean\" must be one of \"locScaleRob\" or \"none\"")
                   
    expect_warning(StdDev(R, portfolio_method="component", weights=c(0.5, 0.5), SE=TRUE), 
                   "To return SEs, \"portfolio_method\" must be \"single\"")
  }
})

test_that("SemiSD vector handling", {
  skip_on_cran()
  data(edhec)
  v <- as.numeric(edhec[1:50, 1])
  res <- SemiSD(v)
  expect_true(is.numeric(res))
})
