
library(zoo)

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
index(minute_contribution) <- seq(c(ISOdate(2000,1,1)), by="min", length.out=60)

hourly_contribution <- contribution
index(hourly_contribution) <- seq(c(ISOdate(2000,1,1)), by="hour", length.out=60)

daily_contribution <- contribution
index(daily_contribution) <- seq(c(ISOdate(2000,1,1)), by="day", length.out=60)

weekly_contribution <- contribution
index(weekly_contribution) <- seq(c(ISOdate(2000,1,1)), by="week", length.out=60)

quaterly_contribution <- contribution
index(quaterly_contribution) <- seq(c(ISOdate(2000,1,1)), by="quarter", length.out=60)

yearly_contribution <- contribution
index(yearly_contribution) <- seq(c(ISOdate(2000,1,1)), by="year", length.out=60)



## Diferent time frames for edhec, weights, 

one_period_returns <- edhec[c("1999-12-31","2000-01-31"),c(1:11)]
colnames(one_period_returns) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")

one_period_weights <- weights["2000-01-01",]
colnames(one_period_weights) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")

multi_period_returns <- edhec[c("2000-01-31", "2000-02-29", "2000-03-31"),c(1:11)]
colnames(multi_period_returns) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")

multi_period_weights <- c(weights["2000-01-01",], weights["2000-01-01",], weights["2000-01-01",])
colnames(multi_period_weights) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV")
index(multi_period_weights) <- as.Date(c("1999-12-31", "2000-01-31", "2000-02-29"))

# Unit tests defaults
epsilon <- 1e-10

expect_error(to.period.contributions(),
             info = "Missing 'Contributions' data handled correctly")

  if(
     expect_error(to.period.contributions(contribution, "daily"),
                  info = "Invalid periodicity parameters are handled correctly")
  ) { expect_error(to.period.contributions(x),
                   info="Invalid periodicity parameters are handled correctly") 
    }

  if(
  expect_error(to.period.contributions(contribution, "weeks"),
               info="Incompatible periodicity parameters are handled correctly") 

  ){   expect_error(to.period.contributions(contribution, "months"),
                    info="Incompatible periodicity parameters are handled correctly")}

  if(
  expect_error(to.period.contributions(minute_contribution),
               info="Data with too high a periodicity is handled correctly")
  ){   expect_error(to.period.contributions(hourly_contribution),
                    info="Data with too high a periodicity is handled correctly")}



  expect_error(to.period.contributions(yearly_contribution), 
               info="Data with too low a periodicity is handled correctly")

  
  expect_equivalent(periodicity(to.period.contributions(contribution))$scale,
                    "yearly",
                    info = "the main function and periodic versions do not throw an error")
  expect_equivalent(periodicity(to.weekly.contributions(daily_contribution))$scale, 
                                "weekly", 
                                info = "the main function and periodic versions do not throw an error")
  expect_equivalent(periodicity(to.monthly.contributions(daily_contribution))$scale, 
                    "monthly", 
                    info = "the main function and periodic versions do not throw an error")
  expect_equivalent(periodicity(to.monthly.contributions(weekly_contribution))$scale, 
                    "monthly", 
                    info = "the main function and periodic versions do not throw an error")
  expect_equivalent(periodicity(to.quarterly.contributions(weekly_contribution))$scale, 
                    "quarterly", 
                    info = "the main function and periodic versions do not throw an error")
  expect_equivalent(periodicity(to.quarterly.contributions(contribution))$scale, 
                    "quarterly", 
                    info = "the main function and periodic versions do not throw an error")
  expect_equivalent(periodicity(to.yearly.contributions(quaterly_contribution))$scale, 
                    "yearly", 
                    info = "the main function and periodic versions do not throw an error")
  expect_equivalent(periodicity(to.yearly.contributions(contribution))$scale, 
                    "yearly", 
                    info = "the main function and periodic versions do not throw an error") 
  expect_equivalent(periodicity(to.quarterly.contributions(weekly_contribution))$scale, 
                    "quarterly",
                    info = "the main function and periodic versions do not throw an error") 

  
# Check first API in underlying PerformanceAnalytics package
expected_contribution_data <- coredata(one_period_weights)*coredata(one_period_returns[2,])
computed_contribution_data <- PerformanceAnalytics::Return.portfolio(one_period_returns, 
                                                                     one_period_weights, 
                                                                     contribution = TRUE)

expect_equal(coredata(expected_contribution_data), 
             coredata(computed_contribution_data[ ,colnames(expected_contribution_data)]), 
             tolerance=epsilon,
             info = "one-period contributions are computed correctly")
  
## Check alternate API in underlying PerformanceAnalytics package
  computed_contribution_data <- PerformanceAnalytics::Return.portfolio(one_period_returns, 
                                                                       one_period_weights, 
                                                                       verbose = TRUE)$contribution
   
  expect_equal(coredata(expected_contribution_data), 
               coredata(computed_contribution_data[,colnames(expected_contribution_data)]), 
               tolerance=epsilon,
               info = "one-period contributions are computed correctly")
  
### Now check the actual API for multi-period contribution but with only one period of data passed in
  computed_multi_period_contribution_data <- to.period.contributions(computed_contribution_data)
    
   expect_equal(coredata(expected_contribution_data), 
                coredata(computed_multi_period_contribution_data[,colnames(expected_contribution_data)]), 
                tolerance=epsilon,
                info = "one-period contributions are computed correctly")
   

   # Check API in underlying PerformanceAnalytics package
  first_period_contribution_data <- coredata(multi_period_weights[1,])*coredata(multi_period_returns[1,])
  second_period_contribution_data <- coredata(multi_period_weights[2,])*coredata(multi_period_returns[2,])
  third_period_contribution_data <- coredata(multi_period_weights[3,])*coredata(multi_period_returns[3,])
  expected_contribution_data <- first_period_contribution_data + second_period_contribution_data*(1+sum(first_period_contribution_data)) + third_period_contribution_data*(1+sum(first_period_contribution_data))*(1+sum(second_period_contribution_data))
  computed_contribution_data <- PerformanceAnalytics::Return.portfolio(multi_period_returns, multi_period_weights, verbose = TRUE)$contribution
  
  expect_equal(first_period_contribution_data, 
               coredata(computed_contribution_data[1,colnames(first_period_contribution_data)]), 
               tolerance=epsilon,
               info = "multi-period contributions with a periodically rebalanced portfolio are computed correctly")
  
  expect_equal(second_period_contribution_data, 
               coredata(computed_contribution_data[2,colnames(second_period_contribution_data)]), 
               tolerance=epsilon,
               info = "multi-period contributions with a periodically rebalanced portfolio are computed correctly")
  
  expect_equal(third_period_contribution_data, 
               coredata(computed_contribution_data[3,colnames(third_period_contribution_data)]), 
               tolerance=epsilon,
               info = "multi-period contributions with a periodically rebalanced portfolio are computed correctly")
 
   # Now check the actual API for multi-period contribution
  computed_multi_period_contribution_data <- to.period.contributions(computed_contribution_data)
  expect_equal(coredata(expected_contribution_data), 
               coredata(computed_multi_period_contribution_data[,colnames(expected_contribution_data)]), tolerance=epsilon)
  