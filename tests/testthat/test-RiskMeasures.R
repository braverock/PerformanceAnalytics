library(PerformanceAnalytics)

test_that("VaR and ES (ETL) work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  methods <- c("historical", "gaussian", "modified")
  
  for (m in methods) {
    # single var
    res_var <- VaR(R, method = m)
    expect_true(is.numeric(res_var))
    
    # single es
    res_es <- ES(R, method = m)
    expect_true(is.numeric(res_es))
    
    # portfolio var
    res_pvar <- VaR(R, method = m, portfolio_method = "component")
    expect_true(is.list(res_pvar) || is.numeric(res_pvar))
    
    # portfolio es
    res_pes <- ES(R, method = m, portfolio_method = "component")
    expect_true(is.list(res_pes) || is.numeric(res_pes))
  }
})

test_that("DownsideDeviation works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res <- DownsideDeviation(R, MAR = 0)
  expect_true(is.numeric(res))
  
  res2 <- DownsideDeviation(R, method = "subset")
  expect_true(is.numeric(res2))
})

test_that("BurkeRatio works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res <- BurkeRatio(R)
  expect_true(is.numeric(res))
  
  res2 <- BurkeRatio(R, modified = TRUE)
  expect_true(is.numeric(res2))
})

test_that("SharpeRatio works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res <- SharpeRatio(R)
  expect_true(is.numeric(res) || is.matrix(res))
  
  res2 <- SharpeRatio.annualized(R)
  expect_true(is.numeric(res2) || is.matrix(res2))
})

test_that("Omega works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res <- Omega(R)
  expect_true(is.numeric(res) || is.matrix(res))
})

test_that("lpm works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  res1 <- lpm(R, n=1, about_mean=FALSE)
  res2 <- lpm(R, n=2, about_mean=TRUE)
  
  expect_true(is.data.frame(res1))
  expect_true(is.data.frame(res2))
})

test_that("DownsideDeviation and SortinoRatio handle vector MAR correctly (#191)", {
  set.seed(123)
  Rand_EQ <- rnorm(100)
  Rand_RF <- rnorm(100, mean = 0.01)

  res1 <- DownsideDeviation(Rand_EQ, MAR = Rand_RF)
  expect_true(is.numeric(res1))
  
  res2 <- SortinoRatio(Rand_EQ, MAR = Rand_RF)
  expect_true(is.numeric(res2))
})
