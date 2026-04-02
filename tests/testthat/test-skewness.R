library(PerformanceAnalytics)
test_that("Fisher skewness correctly evaluates centered moments (fixes #125)", {
  skip_on_cran()

  set.seed(123)
  # Data heavily shifted off-center to maximize variance difference between raw and centered moments
  x <- rnorm(100, mean = 10, sd = 2)

  # 1. Fisher Skewness test
  sk_pa <- PerformanceAnalytics::skewness(x, method = "fisher")

  # Manual calculation of Fisher Skewness (equivalent to e1071::skewness type=2)
  n <- length(x)
  y <- x - mean(x)
  m2 <- sum(y^2) / n
  m3 <- sum(y^3) / n
  sk_ref <- (m3 / m2^(1.5)) * (sqrt(n * (n - 1)) / (n - 2))

  expect_equal(as.numeric(sk_pa), sk_ref, tolerance = 1e-8)
})

test_that("Fisher kurtosis correctly evaluates centered moments (fixes #125)", {
  skip_on_cran()

  set.seed(123)
  x <- rnorm(100, mean = 10, sd = 2)

  ku_pa <- PerformanceAnalytics::kurtosis(x, method = "fisher")

  # Manual calculation of Fisher Excess Kurtosis (equivalent to e1071::kurtosis type=2)
  n <- length(x)
  y <- x - mean(x)
  m2 <- sum(y^2) / n
  m4 <- sum(y^4) / n
  ku_ref <- ((n + 1) * (n - 1) * ((m4 / m2^2) - (3 * (n - 1) / (n + 1)))) / ((n - 2) * (n - 3))

  expect_equal(as.numeric(ku_pa), ku_ref, tolerance = 1e-8)
})
