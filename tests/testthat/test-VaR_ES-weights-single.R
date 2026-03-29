library(PerformanceAnalytics)
test_that("VaR and ES with weights and portfolio_method='single'", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  mu <- as.numeric(colMeans(R))
  sigma <- as.matrix(cov(R))
  m3 <- as.matrix(M3.MM(R))
  m4 <- as.matrix(M4.MM(R))
  
  res_var_hist <- VaR(R, method="historical", portfolio_method="single", weights=w)
  expect_true(is.matrix(res_var_hist))
  
  res_var_gaus <- VaR(R, method="gaussian", portfolio_method="single", weights=w, mu=mu, sigma=sigma)
  expect_true(is.matrix(res_var_gaus))
  
  res_var_mod <- VaR(R, method="modified", portfolio_method="single", weights=w, mu=mu, sigma=sigma, m3=m3, m4=m4)
  expect_true(is.matrix(res_var_mod))
  
  res_es_gaus <- ES(R, method="gaussian", portfolio_method="single", weights=w, mu=mu, sigma=sigma)
  expect_true(is.matrix(res_es_gaus))
  
  res_es_mod <- ES(R, method="modified", portfolio_method="single", weights=w, mu=mu, sigma=sigma, m3=m3, m4=m4)
  expect_true(is.matrix(res_es_mod))
  
  # method kernel portfolio
  if (exists("VaR.kernel.portfolio", asNamespace("PerformanceAnalytics"))) {
     res_kernel <- PerformanceAnalytics:::VaR.kernel.portfolio(R, p=0.95, w=w)
     expect_true(is.list(res_kernel) || is.numeric(res_kernel))
  }
  if (exists("ES.kernel.portfolio", asNamespace("PerformanceAnalytics"))) {
     res_kernel2 <- PerformanceAnalytics:::ES.kernel.portfolio(R, p=0.95, w=w)
     expect_true(is.numeric(res_kernel2) || is.list(res_kernel2))
  }
})
