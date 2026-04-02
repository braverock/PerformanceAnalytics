library(PerformanceAnalytics)

test_that("CAPM.jensenAlpha safely handles time-varying Rf and series parameters", {
  skip_on_cran()
  
  data(managers)
  Ra <- managers[, 1, drop=FALSE]
  Rb <- managers[, 8, drop=FALSE]
  Rf_scalar <- 0.035/12
  Rf_vec <- managers[, 10, drop=FALSE]
  
  # 1. Ensure scalar Alpha is identical to manual replication for vector Rf
  period <- Frequency(Ra)
  Rp <- (prod(1 + Ra, na.rm=TRUE))^(period / sum(!is.na(Ra))) - 1
  Rpb <- (prod(1 + Rb, na.rm=TRUE))^(period / sum(!is.na(Rb))) - 1
  Rpf <- (prod(1 + Rf_vec, na.rm=TRUE))^(period / sum(!is.na(Rf_vec))) - 1
  beta <- CAPM.beta(Ra, Rb, Rf=Rf_vec)
  expected_alpha <- Rp - Rpf - beta * (Rpb - Rpf)
  
  out_alpha <- CAPM.jensenAlpha(Ra, Rb, Rf_vec)
  expect_equal(as.numeric(out_alpha), expected_alpha)
  
  # 2. Ensure series = TRUE yields a time series of dimension aligned with Ra
  out_series <- CAPM.jensenAlpha(Ra, Rb, Rf_vec, series=TRUE)
  expect_true(is.xts(out_series))
  expect_equal(nrow(out_series), nrow(Ra))
  
  # 3. Ensure multi-column Ra gracefully executes
  Ra_multi <- managers[, 1:2]
  out_multi <- CAPM.jensenAlpha(Ra_multi, Rb, Rf_vec)
  expect_equal(dim(out_multi), c(1, 2))
})
