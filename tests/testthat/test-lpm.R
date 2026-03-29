library(PerformanceAnalytics)

test_that("lpm tests", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res1 <- lpm(R[,1], n=1, MAR=0.01)
  res2 <- lpm(R[,1], n=2, MAR=0.01)
  res3 <- lpm(R[,1], n=3, MAR=0.01)
  
  expect_true(is.numeric(res1) || is.data.frame(res1))
  expect_true(is.numeric(res2) || is.data.frame(res2))
  expect_true(is.numeric(res3) || is.data.frame(res3))
})
