library(tinytest)
library(zoo)

# Mock default test objects
data(managers, package = "PerformanceAnalytics")

contribution <- PerformanceAnalytics::Return.portfolio(managers["2002::",1:5],
                                                       weights=c(.05,.1,.3,.4,.15),
                                                       rebalance_on = "quarters",
                                                       verbose=TRUE)$contribution

daily_contribution <- contribution
index(daily_contribution) <- seq(c(ISOdate(2000,1,1)), by="day", length.out=60)

weekly_contribution <- contribution
index(weekly_contribution) <- seq(c(ISOdate(2000,1,1)), by="week", length.out=60)


# Tests that fail
expect_equivalent(periodicity(to.quarterly.contributions(daily_contribution))$scale, 
                  "quarterly", 
                  info = "the main function and periodic versions do not throw an error")  
expect_equivalent(periodicity(to.yearly.contributions(weekly_contribution))$scale, 
                  "yearly",
                  info = "the main function and periodic versions do not throw an error")
expect_equivalent(periodicity(to.yearly.contributions(daily_contribution))$scale, 
                  "yearly", 
                  info = "the main function and periodic versions do not throw an error")