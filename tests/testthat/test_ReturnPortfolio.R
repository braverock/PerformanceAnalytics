library(testthat)
library(xts)
context("Return.portfolio functions")
epsilon = 1e-10


# ----------------------------------------------------------

#
# Mock objects
#

data(test_weights, package = "PerformanceAnalytics")
data(test_returns, package = "PerformanceAnalytics")


# ----------------------------------------------------------

#
# Unit tests
#

expected_returns = rowSums(coredata(test_weights)*coredata(test_returns))

test_that("Geometric Return computations by internal API returns expected results" , {
  # The test_weights in this example do not sum up to 1
  returns = PerformanceAnalytics:::Return.portfolio.geometric(test_returns, test_weights, 
                                                              wealth.index = TRUE, verbose = TRUE)
  expect_equal(as.vector(returns$returns), expected_returns, tolerance = epsilon)
  # Ensure it still works even with a initial portfolio value specified
  returns = PerformanceAnalytics:::Return.portfolio.geometric(test_returns, test_weights, 
                                                              wealth.index = TRUE, value=500, verbose = TRUE)
  expect_equal(as.vector(returns$returns), expected_returns, tolerance = epsilon)
})


test_that("Return computations using main API returns expected results" , {
  # The test_weights in this example do not sum up to 1
  returns = PerformanceAnalytics::Return.portfolio(test_returns, test_weights, 
                                                   wealth.index = TRUE, geometric = TRUE, value=500, verbose = TRUE)
  expect_equal(as.vector(returns$returns), expected_returns, tolerance = epsilon)
  returns = PerformanceAnalytics::Return.portfolio(test_returns, test_weights, 
                                                   wealth.index = TRUE, geometric = FALSE, value=9000, verbose = TRUE)
  expect_equal(as.vector(returns$returns), expected_returns, tolerance = epsilon)
})
