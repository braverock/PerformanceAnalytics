library(PerformanceAnalytics)
test_that("M2, M3, M4 ewma with last.M options work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  # pass last.M2, M3, M4
  last_m2 <- cov(R)
  expect_true(is.matrix(M2.ewma(R, lambda=0.94, last.M2=last_m2)))
  
  # For M3, M4 it probably takes last.M3 etc
  if (exists("M3.ewma")) {
    last_m3 <- M3.MM(R)
    expect_true(is.matrix(M3.ewma(R, lambda=0.94, last.M3=last_m3, as.mat=TRUE)))
    expect_true(is.vector(M3.ewma(R, lambda=0.94, last.M3=last_m3, as.mat=FALSE)))
  }
  
  if (exists("M4.ewma")) {
    last_m4 <- M4.MM(R)
    expect_true(is.matrix(M4.ewma(R, lambda=0.94, last.M4=last_m4, as.mat=TRUE)))
    expect_true(is.vector(M4.ewma(R, lambda=0.94, last.M4=last_m4, as.mat=FALSE)))
  }
})
