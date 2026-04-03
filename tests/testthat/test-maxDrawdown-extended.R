library(PerformanceAnalytics)

test_that("maxDrawdown structurally manages base ts and numeric vectors correctly (fixes #57)", {
  skip_on_cran()
  
  set.seed(123)
  raw_vals <- rnorm(10, mean=0.01)
  
  # A. Base numeric vector evaluated perfectly without crashing
  expect_error(res_vec <- maxDrawdown(raw_vals), NA)
  
  # B. Univariate `ts` evaluated seamlessly (previously crashed evaluating logical(0) column length)
  ts_uni <- ts(raw_vals, start=c(2020, 1), frequency=12)
  expect_error(res_ts <- maxDrawdown(ts_uni), NA)
  
  # C. Multivariate `ts` object correctly iterating over columns
  ts_multi <- ts(matrix(c(raw_vals, rnorm(10)), ncol=2), start=c(2020, 1), frequency=12)
  expect_error(res_multi <- maxDrawdown(ts_multi), NA)
  
  # D. Verification that the single-column evaluations calculate identical mathematical outputs
  expect_equal(as.numeric(res_vec), as.numeric(res_ts))
  expect_equal(dim(res_multi), c(1, 2))
})
