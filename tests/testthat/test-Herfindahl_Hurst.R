library(PerformanceAnalytics)

test_that("Herfindahl and Hurst indices work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:3]
  
  if (exists("HerfindahlIndex", asNamespace("PerformanceAnalytics"))) {
    expect_true(is.numeric(PerformanceAnalytics:::HerfindahlIndex(R)))
  }
  if (exists("HurstIndex", asNamespace("PerformanceAnalytics"))) {
    expect_true(is.numeric(PerformanceAnalytics:::HurstIndex(R)))
  }
})

test_that("CalculateReturns works", {
  skip_on_cran()
  data(prices)
  p <- prices[1:50, 1]
  
  if (exists("CalculateReturns", asNamespace("PerformanceAnalytics"))) {
    expect_s3_class(PerformanceAnalytics:::CalculateReturns(p), "zoo")
    expect_s3_class(PerformanceAnalytics:::CalculateReturns(p, method="log"), "zoo")
    expect_s3_class(PerformanceAnalytics:::CalculateReturns(p, method="difference"), "zoo")
  }
})

test_that("Return.read works", {
  skip_on_cran()
  
  tf <- tempfile(fileext=".csv")
  write.csv(data.frame(Date=as.character(Sys.Date()-1:10), V1=rnorm(10), V2=rnorm(10)), file=tf, row.names=FALSE)
  
  expect_true(is.xts(Return.read(tf, frequency="d")))
  unlink(tf)
})
