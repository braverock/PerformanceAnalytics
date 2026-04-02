library(PerformanceAnalytics)
test_that("Fisher skewness correctly evaluates centered moments (fixes #125)", {
  skip_on_cran()
  skip_if_not_installed("e1071")
  
  set.seed(123)
  # Data heavily shifted off-center to maximize variance difference between raw and centered moments
  x <- rnorm(100, mean=10, sd=2)
  
  # 1. Fisher Skewness test
  sk_pa <- PerformanceAnalytics::skewness(x, method="fisher")
  sk_ref <- e1071::skewness(x, type=2)
  expect_equal(as.numeric(sk_pa), sk_ref, tolerance=1e-8)
  
  # 2. Sample Skewness test (type 3 or G1 vs g1)
  # PerformanceAnalytics 'sample' is G1 (Type 2 in e1071)? 
  # Let's see: PerformanceAnalytics sample = sum((x-mean(x))^3/sqrt(var(x)*(n-1)/n)^3)*n/((n-1)*(n-2))
  # This is precisely G1 (Type 2)! Wait, if 'sample' is also G1, then 'fisher' and 'sample' are identical!
  # Actually, the original RMetrics port might have considered 'fisher' as something else, but they are algebraically equivalent.
})

test_that("Fisher kurtosis correctly evaluates centered moments (fixes #125)", {
  skip_on_cran()
  skip_if_not_installed("e1071")
  
  set.seed(123)
  x <- rnorm(100, mean=10, sd=2)
  
  # e1071 kurtosis defaults to excess kurtosis. 
  # Fisher kurtosis in PerformanceAnalytics IS excess kurtosis (subtracts the 3 factor)
  ku_pa <- PerformanceAnalytics::kurtosis(x, method="fisher")
  ku_ref <- e1071::kurtosis(x, type=2)
  expect_equal(as.numeric(ku_pa), ku_ref, tolerance=1e-8)
})
