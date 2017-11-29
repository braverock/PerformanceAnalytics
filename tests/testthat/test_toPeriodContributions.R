library(testthat)
library(zoo)
context("to.period.contributions function")
epsilon = 1e-10

# ----------------------------------------------------------

#
# Mock objects
#

data(managers, package = "PerformanceAnalytics")
contribution = PerformanceAnalytics::Return.portfolio(managers["2002::",1:5],
                                                      weights=c(.05,.1,.3,.4,.15),
                                                      rebalance_on = "quarters",
                                                      verbose=TRUE)$contribution
malformed_contribution = coredata(contribution)
minute_contribution = contribution
index(minute_contribution) = seq(c(ISOdate(2000,1,1)), by="min", length.out=60)
hourly_contribution = contribution
index(hourly_contribution) = seq(c(ISOdate(2000,1,1)), by="hour", length.out=60)
daily_contribution = contribution
index(daily_contribution) = seq(c(ISOdate(2000,1,1)), by="day", length.out=60)
weekly_contribution = contribution
index(weekly_contribution) = seq(c(ISOdate(2000,1,1)), by="week", length.out=60)
quaterly_contribution = contribution
index(quaterly_contribution) = seq(c(ISOdate(2000,1,1)), by="quarter", length.out=60)
yearly_contribution = contribution
index(yearly_contribution) = seq(c(ISOdate(2000,1,1)), by="year", length.out=60)

data(edhec, package = "PerformanceAnalytics")
data(weights, package = "PerformanceAnalytics")
one_period_returns = edhec["1999-12-31",c(1:11)]
one_period_returns = edhec[c("1999-12-31","2000-01-31"),c(1:11)]
colnames(one_period_returns) = c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
one_period_weights = weights["2000-01-01",]
colnames(one_period_weights) = c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
one_period_returns = edhec[c("1999-12-31","2000-01-31"),c(1:11)]
colnames(one_period_returns) = c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
one_period_weights = weights["2000-01-01",]
colnames(one_period_weights) = c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
multi_period_returns = edhec[c("2000-01-31", "2000-02-29", "2000-03-31"),c(1:11)]
colnames(multi_period_returns) = c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
multi_period_weights = c(weights["2000-01-01",], weights["2000-01-01",], weights["2000-01-01",])
colnames(multi_period_weights) = c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
index(multi_period_weights) = as.Date(c("1999-12-31", "2000-01-31", "2000-02-29"))


# ----------------------------------------------------------

#
# Unit tests
#

test_that("missing are handled correctly", {
  expect_error(to.period.contributions())
})

test_that("invalid parameters are handled correctly", {
  expect_error(to.period.contributions(contribution, "daily"))
  expect_error(to.period.contributions(malformed_contribution))
})

test_that("incompatible parameters are handled correctly", {
  expect_error(to.period.contributions(contribution, "weeks"))
  expect_error(to.period.contributions(contribution, "months"))
})

test_that("data with too high a periodicity is handled correctly", {
  expect_error(to.period.contributions(minute_contribution))
  expect_error(to.period.contributions(hourly_contribution))
})

test_that("data with too low a periodicity is handled correctly", {
  expect_error(to.period.contributions(yearly_contribution))
})

test_that("the main function and periodic versions do not throw an error", {
  expect_error(to.period.contributions(contribution), NA)
  expect_error(to.weekly.contributions(daily_contribution), NA)
  expect_error(to.monthly.contributions(daily_contribution), NA)
  expect_error(to.monthly.contributions(weekly_contribution), NA)
  expect_error(to.quarterly.contributions(daily_contribution), NA)
  expect_error(to.quarterly.contributions(weekly_contribution), NA)
  expect_error(to.quarterly.contributions(contribution), NA)
  expect_error(to.yearly.contributions(daily_contribution), NA)
  expect_error(to.yearly.contributions(weekly_contribution), NA)
  expect_error(to.yearly.contributions(quaterly_contribution), NA)
  expect_error(to.yearly.contributions(contribution), NA)
})


test_that("one-period contributions are computed correctly", {
  expected_contribution_data = coredata(one_period_weights)*coredata(one_period_returns[2,])

  # Check first API in underlying PerformanceAnalytics package
  computed_contribution_data = PerformanceAnalytics::Return.portfolio(one_period_returns, one_period_weights, contribution = TRUE)
  expect_equal(coredata(expected_contribution_data), coredata(computed_contribution_data[,-1]), tolerance=epsilon)

  # Check alternate API in underlying PerformanceAnalytics package
  computed_contribution_data = PerformanceAnalytics::Return.portfolio(one_period_returns, one_period_weights, verbose = TRUE)$contribution
  expect_equal(coredata(expected_contribution_data), coredata(computed_contribution_data), tolerance=epsilon)

  # Now check the actual API for multi-period contribution but with only one period of data passed in
  computed_multi_period_contribution_data = to.period.contributions(computed_contribution_data)
  expect_equal(coredata(expected_contribution_data), coredata(computed_multi_period_contribution_data[,-ncol(computed_multi_period_contribution_data)]), tolerance=epsilon)

  })


test_that("multi-period contributions with a periodically rebalanced portfolio are computed correctly", {
  first_period_contribution_data = coredata(multi_period_weights[1,])*coredata(multi_period_returns[1,])
  second_period_contribution_data = coredata(multi_period_weights[2,])*coredata(multi_period_returns[2,])
  third_period_contribution_data = coredata(multi_period_weights[3,])*coredata(multi_period_returns[3,])
  expected_contribution_data = first_period_contribution_data + second_period_contribution_data*(1+sum(first_period_contribution_data)) + third_period_contribution_data*(1+sum(first_period_contribution_data))*(1+sum(second_period_contribution_data))


  # Check API in underlying PerformanceAnalytics package
  computed_contribution_data = PerformanceAnalytics::Return.portfolio(multi_period_returns, multi_period_weights, verbose = TRUE)$contribution
  expect_equal(first_period_contribution_data, coredata(computed_contribution_data[1,]), tolerance=epsilon)
  expect_equal(second_period_contribution_data, coredata(computed_contribution_data[2,]), tolerance=epsilon)
  expect_equal(third_period_contribution_data, coredata(computed_contribution_data[3,]), tolerance=epsilon)


  # Now check the actual API for multi-period contribution

  computed_multi_period_contribution_data = to.period.contributions(computed_contribution_data)
  expect_equal(coredata(expected_contribution_data), coredata(computed_multi_period_contribution_data[,-ncol(computed_multi_period_contribution_data)]), tolerance=epsilon)
})
