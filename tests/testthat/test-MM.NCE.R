library(PerformanceAnalytics)
library(xts)

test_that("MM.NCE core calculations evaluate without errors", {
  skip_on_cran()
  
  data(edhec)
  R <- edhec[1:30, 1:3] * 100
  
  # A. Test default setup with RidgeD
  expect_error(res1 <- MM.NCE(R), NA)
  expect_true(is.matrix(res1$M2nce))
  expect_true(is.matrix(res1$M3nce)) # as.mat=TRUE by default
  expect_true(is.matrix(res1$M4nce))
  
  # B. Test multiple dimension sizes and optimizations
  expect_error(res2 <- MM.NCE(R, k = 0, as.mat = FALSE), NA)
  expect_true(is.vector(res2$M3nce))
  
  expect_error(res3 <- MM.NCE(R, k = 1, W = "Id"), NA)
  expect_error(res4 <- MM.NCE(R, k = 1, W = "D"), NA)
  expect_error(res5 <- MM.NCE(R, k = 1, W = "RidgeI"), NA)
  
  # Ensure the list-based weight matrix branches evaluate
  expect_error(res6 <- MM.NCE(R, k = 1, W = c("D", "D", "D")), NA)
  
  # C. Test objective function branches natively
  expect_error(res_ineq <- MM.NCE(R, k = 1, include.ineq = FALSE), NA)
  
  # D. Test auxiliary helpers explicitly
  expect_error(PerformanceAnalytics:::NCEconstructW(coredata(R), Wid = "Id", alpha = 0.5), NA)
  expect_error(PerformanceAnalytics:::NCEconstructW(coredata(R), Wid = "RidgeI", alpha = 0.5), NA)
  expect_error(PerformanceAnalytics:::NCEconstructW(coredata(R), Wid = c("D", "D", "D")), NA)
  
  expect_error(PerformanceAnalytics:::NCEinitMCA(coredata(R), k = 1), NA)
  expect_error(PerformanceAnalytics:::NCEinitMCA(coredata(R), k = 0), NA)
})
