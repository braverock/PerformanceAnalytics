library(PerformanceAnalytics)
library(xts)

test_that("checkData method='vector' works", {
  skip_on_cran()
  
  # basic numeric vector
  v <- c(1, 2, NA, 4)
  res <- checkData(v, method="vector", na.rm=TRUE)
  expect_equal(length(res), 3)
  expect_true(is.vector(res))
  
  res2 <- checkData(v, method="vector", na.rm=FALSE)
  expect_equal(length(res2), 4)
  expect_true(is.vector(res2))
  
  # data frame > 1 column
  df <- data.frame(a=1:3, b=4:6)
  expect_warning(res3 <- checkData(df, method="vector", quiet=FALSE), "The data provided is not a vector")
  expect_equal(length(res3), 3)
  expect_equal(res3, 1:3)
  
  # using wrapper
  if (exists("checkDataVector", asNamespace("PerformanceAnalytics"))) {
    expect_equal(PerformanceAnalytics:::checkDataVector(df, quiet=TRUE), 1:3)
  }
})

test_that("checkData method='matrix' works", {
  skip_on_cran()
  df <- data.frame(a=1:3, b=4:6)
  res <- checkData(df, method="matrix")
  expect_true(is.matrix(res))
  
  # wrapper
  if (exists("checkDataMatrix", asNamespace("PerformanceAnalytics"))) {
    expect_true(is.matrix(PerformanceAnalytics:::checkDataMatrix(df)))
  }
})

test_that("checkData method='data.frame' works", {
  skip_on_cran()
  m <- matrix(1:4, ncol=2)
  res <- checkData(m, method="data.frame")
  expect_true(is.data.frame(res))
})

test_that("checkData method='zoo' works", {
  skip_on_cran()
  
  # existing xts -> zoo
  x <- xts(1:3, Sys.Date() - 3:1)
  res <- checkData(x, method="zoo")
  expect_s3_class(res, "zoo")
  
  # existing zoo -> zoo
  z <- zoo(1:3, Sys.Date() - 3:1)
  if (exists("checkDataZoo", asNamespace("PerformanceAnalytics"))) {
    res_z <- PerformanceAnalytics:::checkDataZoo(z)
    expect_s3_class(res_z, "zoo")
  }
  
  # matrix -> zoo (with rownames)
  m <- matrix(1:3, ncol=1)
  rownames(m) <- as.character(Sys.Date() - 3:1)
  res_m <- checkData(m, method="zoo")
  expect_s3_class(res_m, "zoo")
  
  # numeric with no names
  num <- as.numeric(1:3)
  res_num <- checkData(num, method="zoo")
  expect_s3_class(res_num, "zoo")
  
  # numeric with names
  num_names <- as.numeric(1:3)
  names(num_names) <- as.character(Sys.Date() - 3:1)
  res_num_names <- checkData(num_names, method="zoo")
  expect_s3_class(res_num_names, "zoo")
})

test_that("checkData method='xts' works", {
  skip_on_cran()
  
  # existing xts
  x <- xts(1:3, Sys.Date() - 3:1)
  expect_s3_class(checkData(x), "xts") # default method is xts
  
  # xtsible object
  z <- zoo(1:3, Sys.Date() - 3:1)
  res_z <- checkData(z)
  expect_s3_class(res_z, "xts")
  
  # !xtsible numeric -> warning and return zoo
  num <- as.numeric(1:3)
  expect_warning(res_num <- checkData(num, quiet=FALSE), "The data cannot be converted into a time series")
  expect_s3_class(res_num, "zoo")
  
  # !xtsible non-numeric -> error
  chars <- c("a", "b", "c")
  expect_error(checkData(chars), "The data cannot be converted into a time series")
})
