library(PerformanceAnalytics)
library(xts)

test_that("Return.annualized respects na.rm parameter (fixes #172)", {
  skip_on_cran()
  
  # A. Test on a plain numeric vector (the specific issue reported by user)
  vec_with_na <- c(0.1, 0.2, NA)
  
  # Default behavior (na.rm = TRUE) should mirror previous functionality and drop the NA
  expect_equal(as.numeric(round(Return.annualized(vec_with_na, scale=1), 7)), 0.1489125)
  
  # New behavior (na.rm = FALSE) should return NA
  expect_true(is.na(Return.annualized(vec_with_na, scale=1, na.rm=FALSE)))
  
  # B. Test on an xts object
  xts_with_na <- xts(vec_with_na, order.by = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")))
  
  expect_equal(as.numeric(round(Return.annualized(xts_with_na, scale=12), 4)), 4.2899)
  expect_true(is.na(as.numeric(Return.annualized(xts_with_na, scale=12, na.rm=FALSE))))
  
  # C. Test multivariate xts object with partial NAs
  multi_xts <- cbind(xts_with_na, xts(c(0.1, 0.2, 0.3), order.by = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))))
  res <- Return.annualized(multi_xts, scale=12, na.rm=FALSE)
  
  # Column 1 should be NA, Column 2 should evaluate successfully
  expect_true(is.na(as.numeric(res[,1])))
  expect_false(is.na(as.numeric(res[,2])))
})
