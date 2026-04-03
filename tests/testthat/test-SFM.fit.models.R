library(PerformanceAnalytics)

test_that("SFM.fit.models works correctly", {
  skip_on_cran()
  if (!requireNamespace("fit.models", quietly = TRUE)) skip("fit.models not installed")
  if (!requireNamespace("RobStatTM", quietly = TRUE)) skip("RobStatTM not installed")
  
  data(managers)
  mgrs <- managers["2002/"]
  # Rename columns as in examples
  colnames(mgrs)[7:10] <- c("LSEQ","SP500","Bond10Yr","RF")
  
  expect_error(res <- SFM.fit.models(mgrs$HAM1, mgrs$SP500, Rf = mgrs$RF, plots=FALSE), NA)
  expect_s3_class(res, "lmfm")
  expect_equal(length(res), 2) # list of 2 models
})
