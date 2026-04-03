library(PerformanceAnalytics)
library(xts)

test_that("table.CalendarReturns matches legacy expectations", {
  # ------------------------------------------------------------------------------
  # Test 1: table.CalendarReturns preserves identical behavior for monthly inputs
  # ------------------------------------------------------------------------------
  data(managers)
  # Managers dataset contains monthly periodicity returns
  res_monthly <- table.CalendarReturns(managers[, 1, drop = FALSE])
  
  expect_s3_class(res_monthly, "data.frame")
  expect_equal(ncol(res_monthly), 13)
  expect_equal(colnames(res_monthly)[13], colnames(managers)[1])
  
  # ------------------------------------------------------------------------------
  # Test 2: table.CalendarReturns successfully converts and outputs daily returns
  # ------------------------------------------------------------------------------
  # Mock 2 years of daily data
  dates <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  
  set.seed(123)
  daily_rets <- xts(rnorm(length(dates), mean = 0.001, sd = 0.01), order.by = dates)
  colnames(daily_rets) <- "Asset_A"
  
  res_daily <- table.CalendarReturns(daily_rets, digits = 2)
  
  # Check dimension shapes and conversions
  expect_s3_class(res_daily, "data.frame")
  expect_equal(ncol(res_daily), 13)
  expect_equal(nrow(res_daily), 2)
  expect_equal(rownames(res_daily), c("2020", "2021"))
  expect_true(!all(is.na(res_daily)))
  
  # ------------------------------------------------------------------------------
  # Test 3: respects geometric vs arithmetic accumulation on daily inputs
  # ------------------------------------------------------------------------------
  # Setup 1 full month of exactly 1% returns per business day
  dates_jan <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
  dates_jan <- dates_jan[!weekdays(dates_jan) %in% c("Saturday", "Sunday")]
  daily_rets_jan <- xts(rep(0.01, length(dates_jan)), order.by = dates_jan)
  colnames(daily_rets_jan) <- "Asset_A"
  
  res_geom <- table.CalendarReturns(daily_rets_jan, geometric = TRUE, as.perc = FALSE, digits = 4)
  res_arith <- table.CalendarReturns(daily_rets_jan, geometric = FALSE, as.perc = FALSE, digits = 4)
  
  # Total days in Jan 2020: 23 weekdays
  n_days <- length(dates_jan)
  
  # Arithmetic calculation (0.01 * 23)
  expected_arith <- round(0.01 * n_days, 4)
  
  # Geometric calculation ((1 + 0.01)^23 - 1)
  expected_geom <- round((1.01 ^ n_days) - 1, 4)
  
  expect_equal(as.numeric(res_arith["2020", "Jan"]), expected_arith)
  expect_equal(as.numeric(res_geom["2020", "Jan"]), expected_geom)
})

test_that("to.period.contributions matches legacy expectations", {
  # Default test objects
  data(managers, package = "PerformanceAnalytics")
  data(edhec, package = "PerformanceAnalytics")
  data(weights, package = "PerformanceAnalytics")
  
  ## Varous periodicities of portfolio return contributions
  contribution <- PerformanceAnalytics::Return.portfolio(managers["2002::",1:5],
                                                        weights=c(.05,.1,.3,.4,.15),
                                                        rebalance_on = "quarters",
                                                        verbose=TRUE)$contribution
  
  malformed_contribution <- coredata(contribution)
  
  minute_contribution <- contribution
  index(minute_contribution) <- seq(c(ISOdate(2000,1,1)), by="min", length.out=nrow(contribution))
  
  hourly_contribution <- contribution
  index(hourly_contribution) <- seq(c(ISOdate(2000,1,1)), by="hour", length.out=nrow(contribution))
  
  daily_contribution <- contribution
  index(daily_contribution) <- seq(c(ISOdate(2000,1,1)), by="day", length.out=nrow(contribution))
  
  weekly_contribution <- contribution
  index(weekly_contribution) <- seq(c(ISOdate(2000,1,1)), by="week", length.out=nrow(contribution))
  
  quaterly_contribution <- contribution
  index(quaterly_contribution) <- seq(c(ISOdate(2000,1,1)), by="quarter", length.out=nrow(contribution))
  
  yearly_contribution <- contribution
  index(yearly_contribution) <- seq(c(ISOdate(2000,1,1)), by="year", length.out=nrow(contribution))
  
  ## Diferent time frames for edhec, weights, 
  one_period_returns <- edhec[c("1999-12-31","2000-01-31"),c(1:11)]
  colnames(one_period_returns) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
  
  one_period_weights <- weights["2000-01-01",]
  colnames(one_period_weights) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
  
  multi_period_returns <- edhec[c("2000-01-31", "2000-02-29", "2000-03-31"),c(1:11)]
  colnames(multi_period_returns) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
  
  multi_period_weights <- rbind(weights["2000-01-01",], weights["2000-01-01",], weights["2000-01-01",])
  colnames(multi_period_weights) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
  index(multi_period_weights) <- as.Date(c("1999-12-31", "2000-01-31", "2000-02-29"))
  
  # Unit tests defaults
  epsilon <- 1e-10
  
  expect_error(to.period.contributions())
  expect_error(to.period.contributions(contribution, "daily"))
  expect_error(to.period.contributions(contribution, "weeks"))
  expect_error(to.period.contributions(minute_contribution))
  expect_error(to.period.contributions(yearly_contribution))
  
  expect_equal(periodicity(to.period.contributions(contribution))$scale, "yearly")
  expect_equal(periodicity(to.weekly.contributions(daily_contribution))$scale, "weekly")
  expect_equal(periodicity(to.monthly.contributions(daily_contribution))$scale, "monthly")
  expect_equal(periodicity(to.monthly.contributions(weekly_contribution))$scale, "monthly")
  expect_equal(periodicity(to.quarterly.contributions(weekly_contribution))$scale, "quarterly")
  expect_equal(periodicity(to.quarterly.contributions(contribution))$scale, "quarterly")
  expect_equal(periodicity(to.yearly.contributions(quaterly_contribution))$scale, "yearly")
  expect_equal(periodicity(to.yearly.contributions(contribution))$scale, "yearly") 
  
  # Check first API in underlying PerformanceAnalytics package
  expected_contribution_data <- coredata(one_period_weights)*coredata(one_period_returns[2,])
  computed_contribution_data <- suppressWarnings(PerformanceAnalytics::Return.portfolio(one_period_returns, 
                                                                       one_period_weights, 
                                                                       contribution = TRUE))
  
  expect_equal(as.numeric(coredata(expected_contribution_data)), 
               as.numeric(coredata(computed_contribution_data[2, colnames(expected_contribution_data)])), 
               tolerance=epsilon)
  
  ## Check alternate API in underlying PerformanceAnalytics package
  computed_contribution_data <- suppressWarnings(PerformanceAnalytics::Return.portfolio(one_period_returns, 
                                                                         one_period_weights, 
                                                                         verbose = TRUE))$contribution
  
  expect_equal(as.numeric(coredata(expected_contribution_data)), 
               as.numeric(coredata(computed_contribution_data[2, colnames(expected_contribution_data)])), 
               tolerance=epsilon)
  
  ### Now check the actual API for multi-period contribution but with only one period of data passed in
  computed_multi_period_contribution_data <- to.period.contributions(computed_contribution_data)
  
  expect_equal(as.numeric(coredata(expected_contribution_data)), 
               as.numeric(coredata(computed_multi_period_contribution_data[2, colnames(expected_contribution_data)])), 
               tolerance=epsilon)
  
  # Check API in underlying PerformanceAnalytics package
  first_period_contribution_data <- coredata(multi_period_weights[1,])*coredata(multi_period_returns[1,])
  second_period_contribution_data <- coredata(multi_period_weights[2,])*coredata(multi_period_returns[2,])
  third_period_contribution_data <- coredata(multi_period_weights[3,])*coredata(multi_period_returns[3,])
  expected_contribution_data <- first_period_contribution_data + second_period_contribution_data*(1+sum(first_period_contribution_data)) + third_period_contribution_data*(1+sum(first_period_contribution_data))*(1+sum(second_period_contribution_data))
  computed_contribution_data <- PerformanceAnalytics::Return.portfolio(multi_period_returns, multi_period_weights, verbose = TRUE)$contribution
  
  expect_equal(as.numeric(first_period_contribution_data), 
               as.numeric(coredata(computed_contribution_data[1,colnames(first_period_contribution_data)])), 
               tolerance=epsilon)
  
  expect_equal(as.numeric(second_period_contribution_data), 
               as.numeric(coredata(computed_contribution_data[2,colnames(second_period_contribution_data)])), 
               tolerance=epsilon)
  
  expect_equal(as.numeric(third_period_contribution_data), 
               as.numeric(coredata(computed_contribution_data[3,colnames(third_period_contribution_data)])), 
               tolerance=epsilon)
  
  # Now check the actual API for multi-period contribution
  computed_multi_period_contribution_data <- to.period.contributions(computed_contribution_data)
  expect_equal(as.numeric(coredata(expected_contribution_data)), 
               as.numeric(coredata(computed_multi_period_contribution_data[,colnames(expected_contribution_data)])), tolerance=epsilon)
})
