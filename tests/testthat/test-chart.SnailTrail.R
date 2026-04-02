library(PerformanceAnalytics)

test_that("chart.SnailTrail handles NA sequences and short series safely", {
  skip_on_cran()

  # Suppress plot generation visually since we are testing logic completion
  pdf(file = NULL)

  # 1. Entirely NA series
  x_na <- xts::xts(matrix(NA, ncol = 1, nrow = 10), as.Date("2020-01-01") + 1:10)
  expect_warning(expect_error(chart.SnailTrail(x_na), NA), "The first column is entirely NA")

  # 2. Series shorter than stepsize (width=12)
  x_short <- xts::xts(matrix(rnorm(5), ncol = 1, nrow = 5), as.Date("2020-01-01") + 1:5)
  expect_warning(expect_error(chart.SnailTrail(x_short), NA), "Not enough data")

  # 3. Multi-column series where ONE column is too short, but another is valid
  x_mixed <- xts::xts(cbind(rnorm(24), c(rep(NA, 19), rnorm(5))), as.Date("2020-01-01") + 1:24)
  expect_error(chart.SnailTrail(x_mixed), NA)

  dev.off()
})

test_that("chart.SnailTrail successfully renders on valid data", {
  skip_on_cran()
  
  data(edhec)
  R <- edhec[1:50, 1:3]
  
  pdf(file=NULL)
  
  # Standard execution
  expect_error(chart.SnailTrail(R), NA)
  
  # Varying add.names parameters
  expect_error(chart.SnailTrail(R, add.names = "lastonly"), NA)
  expect_error(chart.SnailTrail(R, add.names = "firstandlast"), NA)
  expect_error(chart.SnailTrail(R, add.names = "all"), NA)
  expect_error(chart.SnailTrail(R, add.names = "none"), NA)
  
  # Adjusting stepsize and width
  expect_error(chart.SnailTrail(R, width=24, stepsize=6), NA)
  
  dev.off()
})

test_that("chart.SnailTrail handles overlapping / mismatched start dates natively", {
  skip_on_cran()
  
  data(edhec)
  # Create a matrix where the second column starts 2 years later
  R_mismatched <- edhec[1:60, 1:2]
  R_mismatched[1:24, 2] <- NA
  
  pdf(file=NULL)
  # Should not crash on subscript indexing when padding maxrows
  expect_error(chart.SnailTrail(R_mismatched), NA)
  dev.off()
})
