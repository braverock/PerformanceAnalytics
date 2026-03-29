library(PerformanceAnalytics)

test_that("M4.shrink structured matrix returns", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  f <- rowSums(R)
  
  res <- M4.shrink(R, targets=c(1,2,3,4), f=f, as.mat=TRUE)
  expect_true(is.list(res))
})
