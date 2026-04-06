library(PerformanceAnalytics)
library(xts)

test_that("chart.VaRSensitivity evaluates risk methods and correctly passes moments", {
  skip_on_cran()
  data(managers)
  R <- managers[, 1, drop=FALSE]
  
  pdf(file = NULL)
  dev.control(displaylist = "enable")
  on.exit(dev.off())
  
  # Standard call
  expect_s3_class(chart.VaRSensitivity(R, methods=c("ModifiedVaR")), "recordedplot")

  # Call passing explicit moments
  # Pre-calculate explicit moments that deviate from standard moments
  mu <- 0.05
  sigma <- 0.10
  m3 <- -1.5 # high negative skew
  m4 <- 8.0  # high kurtosis
  
  # Ensure no error when passing explicit moments and other valid VaR arguments
  # along with standard plot parameters like lwd
  expect_error(
    chart.VaRSensitivity(R, methods=c("ModifiedVaR", "ModifiedES"), 
                         mu=mu, sigma=sigma, m3=m3, m4=m4, 
                         invert=FALSE, lwd=2), 
    NA)
})
