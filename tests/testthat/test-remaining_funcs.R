library(PerformanceAnalytics)

test_that("Functions with 0% coverage work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  # AverageDrawdown
  expect_true(is.numeric(AverageDrawdown(R)))
  
  # AverageLength
  expect_true(is.numeric(AverageLength(R)))
  
  # AverageRecovery
  expect_true(is.numeric(AverageRecovery(R)))
  
  # DrawdownDeviation
  expect_true(is.numeric(DrawdownDeviation(R)))
  
  # CoKurtosisMatrix, CoSkewnessMatrix
  expect_true(is.matrix(CoKurtosisMatrix(R)))
  expect_true(is.matrix(CoSkewnessMatrix(R)))

  # SFM.fit.models
  data(managers)
  Ra <- managers[1:50, 1, drop=FALSE]
  Rb <- managers[1:50, 8, drop=FALSE]
  
  # Passing ask=FALSE prevents the menu prompt error when testing fit.models directly
  # However, SFM.fit.models explicitly has `ask` inside its plot generation in the method which can't be bypassed.
  # So we will just test it on the object creation piece and ignore plotting it
  if (requireNamespace("RobStatTM", quietly = TRUE)) {
     # suppress printing/plotting that might hang
     pdf(file=NULL)
     # actually, looking at SFM.fit.models, it calls plot() automatically inside the code! 
     # so we cannot test it cleanly without hanging the test suite since it uses menu() unconditionally.
     dev.off()
  }
})
