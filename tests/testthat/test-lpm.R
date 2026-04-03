library(PerformanceAnalytics)
library(xts)

test_that("lpm natively avoids xts indexing crashes and gracefully warns on missing values", {
  skip_on_cran()
  data(edhec)

  R <- edhec[1:50, 1:2]

  # A. Standard evaluation (should not error or warn on valid subset threshold)
  # Provide a reasonable threshold
  expect_error(res <- lpm(R, threshold = 0.0), NA)
  expect_true(is.data.frame(res))
  expect_equal(dim(res), c(1, 2))

  # B. Extreme threshold with NO values meeting the mask
  # Ensure the system outputs an informative warning as intended
  expect_warning(res_missing <- lpm(R, threshold = -1.0), "No observations below the threshold for column")
  expect_equal(as.numeric(res_missing), c(NA_real_, NA_real_))

  # C. about_mean = TRUE logic works
  expect_error(res_mean <- lpm(R, threshold = 0.0, about_mean = TRUE), NA)
  expect_true(is.numeric(res_mean[[1]]))
})

test_that("lpm gracefully handles severe edge cases and bounds (fixes #60)", {
  skip_on_cran()

  data(edhec)
  R <- edhec[1:50, 1:2]

  # 1. Entirely NA columns
  R_na <- R
  R_na[, 2] <- NA
  expect_warning(res_na <- lpm(R_na, threshold = 0.0), "No observations below the threshold for column CTA Global")
  expect_equal(as.numeric(res_na[[2]]), NA_real_)
  expect_true(is.numeric(res_na[[1]]))

  # 2. Invalid 'n' arguments (e.g. NA or negative)
  expect_error(lpm(R, n = NA, threshold = 0.0), "n must be a valid numeric value.")

  # 3. Invalid threshold (e.g. NA)
  expect_error(lpm(R, threshold = NA), "threshold must be a valid numeric value.")
})
