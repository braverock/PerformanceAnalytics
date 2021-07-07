
library(xts)

# Mock objects
data(test_weights, package = "PerformanceAnalytics")
data(test_returns, package = "PerformanceAnalytics")

# Unit tests Returns.portfolio function. Initial defaults.
expected_returns <- rowSums(coredata(test_weights)*coredata(test_returns))
epsilon = 1e-10

# Test internal API: Does Return.portfolio.geometric returns expected results?
#               AND  Does the same test work with an initial money value?

returns <- PerformanceAnalytics:::Return.portfolio.geometric(test_returns, 
                                                             test_weights, 
                                                             wealth.index =TRUE, 
                                                             verbose =TRUE)

returns_val <- PerformanceAnalytics:::Return.portfolio.geometric(test_returns, 
                                                             test_weights, 
                                                             wealth.index =TRUE, 
                                                             value =100000, 
                                                             verbose =TRUE)

if(expect_equal(as.vector(returns$returns), 
                expected_returns, 
                tolerance = epsilon))  {
  
   expect_equal(as.vector(returns_val$returns), 
                expected_returns, 
                tolerance = epsilon)
}

# Test MAIN API: Does Return.portfolio.geometric returns expected results?
#                AND  Does the same test work with an initial money value?

returns_main <- PerformanceAnalytics:::Return.portfolio(test_returns, 
                                                        test_weights, 
                                                        wealth.index =TRUE, 
                                                        verbose =TRUE)

returns_main_val <- PerformanceAnalytics:::Return.portfolio(test_returns, 
                                                            test_weights, 
                                                            wealth.index =TRUE, 
                                                            value =100000, 
                                                            verbose =TRUE)

if(expect_equal(as.vector(returns_main$returns), 
                expected_returns, 
                tolerance = epsilon))  {
  
  expect_equal(as.vector(returns_main_val$returns), 
               expected_returns, 
               tolerance = epsilon)
}


#if ( Sys.info()[['sysname']] == "Windows"){
#  exit_file("Cannot test this on Windows")
#}
