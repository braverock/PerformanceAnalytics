library(PerformanceAnalytics)
library(xts)

test_that("Return.convert handles checkSeedValue logic", {
  skip_on_cran()
  vec <- c(0.01, 0.02)
  R <- xts(vec, order.by = as.Date("2020-01-01") + 1:2)
  
  # Missing coredata_content -> warns and defaults to discreteReturn
  expect_warning(Return.convert(R, destinationType="discrete"), "object is missing coredata_content attribute")
  
  # Length > 1 seedValue -> warns and uses first
  attr(R, "coredata_content") <- "discreteReturn"
  expect_warning(res <- Return.convert(R, destinationType="discrete", seedValue=c(1,2)), "seedValue length greater than 1")
  
  # Non-numeric seedValue -> stops
  expect_error(Return.convert(R, destinationType="discrete", seedValue="A"), "Require a numeric value for seedValue")
  
  # XTS seedValue -> converts to scalar
  sv_xts <- xts(1, order.by = as.Date("2020-01-01"))
  expect_warning(res_xts <- Return.convert(R, destinationType="discrete", seedValue=sv_xts), "destinationType is same")
})

test_that("Return.convert discreteReturn", {
  skip_on_cran()
  vec <- c(0.01, 0.02, -0.01)
  R <- xts(vec, order.by = as.Date("2020-01-01") + 1:3)
  attr(R, "coredata_content") <- "discreteReturn"
  
  # destinationType multiple
  expect_error(Return.convert(R, destinationType=c("discrete", "log")), "Must select a single destinationType")
  
  # discrete -> discrete
  expect_warning(res1 <- Return.convert(R, destinationType="discrete"), "destinationType is same")
  
  # discrete -> log
  res2 <- Return.convert(R, destinationType="log")
  expect_equal(attr(res2, "coredata_content"), "logReturn")
  
  # discrete -> difference
  expect_warning(res3 <- Return.convert(R, destinationType="difference"), "No seedValue detected for type 'difference'")
  expect_equal(attr(res3, "coredata_content"), "difference")
  
  # discrete -> difference with seedValue
  res3_sv <- Return.convert(R, destinationType="difference", seedValue=1)
  expect_equal(attr(res3_sv, "coredata_content"), "difference")
  
  # discrete -> level
  expect_warning(res4 <- Return.convert(R, destinationType="level"), "Using Level.calculate")
  expect_equal(attr(res4, "coredata_content"), "level")
  
  # discrete -> unknown
  expect_error(Return.convert(R, destinationType="unknown"), "Unknown destinationType")
})

test_that("Return.convert logReturn", {
  skip_on_cran()
  vec <- c(0.01, 0.02, -0.01)
  R <- xts(vec, order.by = as.Date("2020-01-01") + 1:3)
  attr(R, "coredata_content") <- "logReturn"
  
  # log -> discrete
  res1 <- Return.convert(R, destinationType="discrete")
  expect_equal(attr(res1, "coredata_content"), "discreteReturn")
  
  # log -> log
  expect_warning(res2 <- Return.convert(R, destinationType="log"), "destinationType is same")
  
  # log -> difference
  expect_warning(res3 <- Return.convert(R, destinationType="difference"), "No seedValue detected for type 'difference'")
  
  res3_sv <- Return.convert(R, destinationType="difference", seedValue=1)
  expect_equal(attr(res3_sv, "coredata_content"), "difference")
  
  # log -> level
  expect_warning(res4 <- Return.convert(R, destinationType="level"), "Using Level.calculate")
  
  # log -> unknown
  expect_error(Return.convert(R, destinationType="unknown"), "Unknown destinationType")
})

test_that("Return.convert difference", {
  skip_on_cran()
  vec <- c(0.01, 0.02, -0.01)
  R <- xts(vec, order.by = as.Date("2020-01-01") + 1:3)
  attr(R, "coredata_content") <- "difference"
  
  # checkSeedValue stops if seedValue is null for difference
  expect_error(Return.convert(R, destinationType="discrete"), "When calculating levels using 'difference'")
  
  # difference -> discrete
  res1 <- Return.convert(R, destinationType="discrete", seedValue=1)
  expect_equal(attr(res1, "coredata_content"), "discreteReturn")
  
  # difference -> log
  res2 <- Return.convert(R, destinationType="log", seedValue=1)
  expect_equal(attr(res2, "coredata_content"), "logReturn")
  
  # difference -> difference
  expect_warning(res3 <- Return.convert(R, destinationType="difference", seedValue=1), "destinationType is same")
  
  # difference -> level
  expect_warning(res4 <- Return.convert(R, destinationType="level", seedValue=1), "Using Level.calculate")
  
  # difference -> unknown
  expect_error(Return.convert(R, destinationType="unknown", seedValue=1), "Unknown destinationType")
})

test_that("Return.convert level", {
  skip_on_cran()
  vec <- c(1.0, 1.1, 1.2)
  R <- xts(vec, order.by = as.Date("2020-01-01") + 1:3)
  attr(R, "coredata_content") <- "level"
  
  # level -> discrete
  res1 <- Return.convert(R, destinationType="discrete")
  expect_equal(attr(res1, "coredata_content"), "discreteReturn")
  
  # level -> log
  res2 <- Return.convert(R, destinationType="log")
  expect_equal(attr(res2, "coredata_content"), "logReturn")
  
  # level -> difference
  expect_warning(res3 <- Return.convert(R, destinationType="difference"), "No seedValue detected for type 'difference'")
  
  res3_sv <- Return.convert(R, destinationType="difference", seedValue=1)
  expect_equal(attr(res3_sv, "coredata_content"), "difference")
  
  # level -> level
  expect_warning(res4 <- Return.convert(R, destinationType="level"), "destinationType is same")
  
  # level -> unknown
  expect_error(Return.convert(R, destinationType="unknown"), "Unknown destinationType")
})

test_that("Return.convert unknown coredata_content", {
  skip_on_cran()
  vec <- c(0.01, 0.02)
  R <- xts(vec, order.by = as.Date("2020-01-01") + 1:2)
  attr(R, "coredata_content") <- "unknown_type"
  
  expect_error(Return.convert(R, destinationType="discrete"), "Unknown, unsupported or missing coredata_content attribute")
})
