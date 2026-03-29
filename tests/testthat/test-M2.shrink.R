library(PerformanceAnalytics)

test_that("M2.shrink variations", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  f <- rowSums(R)
  
  res <- M2.shrink(R, targets=c(1,2,3), f=f)
  expect_true(is.list(res))
  
  res2 <- M2.shrink(R, targets=c(1,2))
  expect_true(is.list(res2))
  
  res3 <- M2.shrink(R, targets=c(2))
  expect_true(is.list(res3))
})

test_that("M4.shrink variations", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  f <- rowSums(R)
  
  res <- M4.shrink(R, targets=c(1,2,3,4), f=f)
  expect_true(is.list(res))
  
  res2 <- M4.shrink(R, targets=c(1,2))
  expect_true(is.list(res2))
  
  res3 <- M4.shrink(R, targets=c(3), f=f)
  expect_true(is.list(res3))
})
