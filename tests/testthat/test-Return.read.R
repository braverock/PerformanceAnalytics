library(PerformanceAnalytics)
library(xts)

test_that("Return.read evaluates format.in correctly", {
  skip_on_cran()
  
  # Check for error without a filename
  expect_error(Return.read())
  
  tmp_file <- tempfile()
  
  # A. ISO8601 (default format %Y-%m-%d)
  writeLines("Date,Asset1\n2020-01-31,0.05", tmp_file)
  res_iso <- Return.read(tmp_file, format.in="ISO8601", frequency="d")
  expect_true(is.xts(res_iso))
  expect_equal(as.character(index(res_iso)), "2020-01-31")
  
  # B. Excel format (%m/%d/%Y)
  writeLines("Date,Asset1\n01/31/2020,0.05", tmp_file)
  res_xls <- Return.read(tmp_file, format.in="excel", frequency="d")
  expect_equal(as.character(index(res_xls)), "2020-01-31")
  
  # C. OpenOffice format (%m/%d/%y)
  writeLines("Date,Asset1\n01/31/20,0.05", tmp_file)
  res_oo <- Return.read(tmp_file, format.in="oo", frequency="d")
  expect_equal(as.character(index(res_oo)), "2020-01-31")
  
  # D. Gnumeric format (%d-%b-%Y)
  writeLines("Date,Asset1\n31-Jan-2020,0.05", tmp_file)
  res_gnu <- Return.read(tmp_file, format.in="gnumeric", frequency="d")
  expect_equal(as.character(index(res_gnu)), "2020-01-31")
  
  # E. Custom format parameter passthru
  writeLines("Date,Asset1\nJan.31.2020,0.05", tmp_file)
  res_cus <- Return.read(tmp_file, format.in="%b.%d.%Y", frequency="d")
  expect_equal(as.character(index(res_cus)), "2020-01-31")
})

test_that("Return.read evaluates frequency scaling correctly", {
  skip_on_cran()
  
  tmp_file <- tempfile()
  writeLines("Date,Asset1\n2020-01-31,0.05", tmp_file)
  
  # A. Daily (as.Date)
  res_d <- Return.read(tmp_file, frequency="d")
  expect_equal(class(index(res_d)), "Date")
  
  # B. Monthly (as.yearmon)
  res_m <- Return.read(tmp_file, frequency="m")
  expect_equal(class(index(res_m)), "yearmon")
  
  # C. Quarterly (as.yearqtr)
  res_q <- Return.read(tmp_file, frequency="q")
  expect_equal(class(index(res_q)), "yearqtr")
  
  # D. Irregular (as.POSIXct)
  writeLines("Date,Asset1\n2020-01-31 09:30:00,0.05", tmp_file)
  res_i <- Return.read(tmp_file, format.in="%Y-%m-%d %H:%M:%S", frequency="i")
  expect_true(inherits(index(res_i), "POSIXct"))
  
  # E. Other (NULL FUN) 
  writeLines("Date,Asset1\n2020-01-31,0.05", tmp_file)
  # When FUN is NULL, it falls back to whatever read.zoo default is (usually Date or character)
  res_o <- Return.read(tmp_file, frequency="o")
  expect_true(is.xts(res_o) || is.zoo(res_o))
  
  unlink(tmp_file)
})
