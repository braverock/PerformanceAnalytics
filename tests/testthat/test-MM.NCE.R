library(PerformanceAnalytics)
test_that("MM.NCE and NCE options", {
  skip_on_cran()
  data(edhec)
  
  R <- edhec[1:100, 1:3] * 100
  
  # Default
  res1 <- MM.NCE(R)
  expect_type(res1, "list")
  expect_true(all(c("M2nce", "M3nce", "M4nce", "optim.sol") %in% names(res1)))
  
  # as.mat = FALSE
  res2 <- MM.NCE(R, as.mat = FALSE)
  expect_true(is.vector(res2$M3nce))
  expect_true(is.vector(res2$M4nce))
  
  # k specified
  res3 <- MM.NCE(R, k = 1)
  expect_type(res3, "list")
  
  # Ridge weights
  res4 <- MM.NCE(R, W = list("Wid" = "RidgeD", "alpha" = 0.5))
  expect_type(res4, "list")
  
  # Bootstrapped Ridge
  res5 <- MM.NCE(edhec[1:100, 1:2] * 100, W = list("Wid" = "RidgeD", "alpha" = NULL, "nb" = 10, "alphavec" = seq(0.2, 0.6, by = 0.2)))
  expect_type(res5, "list")
})
