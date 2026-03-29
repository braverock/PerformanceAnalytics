library(PerformanceAnalytics)

test_that("M3 and M4 MM.old methods work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:3]
  
  if (exists("M3.MM.old", asNamespace("PerformanceAnalytics"))) {
    m3 <- PerformanceAnalytics:::M3.MM.old(R)
    expect_true(is.numeric(m3) || is.matrix(m3) || is.vector(m3))
  }
  
  if (exists("M4.MM.old", asNamespace("PerformanceAnalytics"))) {
    m4 <- PerformanceAnalytics:::M4.MM.old(R)
    expect_true(is.numeric(m4) || is.matrix(m4) || is.vector(m4))
  }
})
