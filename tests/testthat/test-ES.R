library(PerformanceAnalytics)

test_that("ES methods test", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  
  # Check portfolio component ES (which hits ETL and ES.CornishFisher.portfolio underlying)
  es_comp <- ES(R, method="modified", portfolio_method="component", weights=w)
  expect_true(is.list(es_comp))
  
  es_comp_hist <- ES(R, method="historical", portfolio_method="component", weights=w)
  expect_true(is.list(es_comp_hist))
  
  es_comp_gaus <- ES(R, method="gaussian", portfolio_method="component", weights=w)
  expect_true(is.list(es_comp_gaus))
  
  # Check ETL directly since it is masked/used underneath
  expect_true(is.numeric(ETL(R[,1])))
  expect_true(is.numeric(ETL(R[,1], method="gaussian")))
  expect_true(is.numeric(ETL(R[,1], method="historical")))
  
  # Check single vector matrix inputs
  expect_true(is.numeric(ES(R[,1])))
  
  # Test with invert=FALSE
  expect_true(is.numeric(ES(R[,1], invert=FALSE)))
})

test_that("VaR methods test", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  
  # Portfolio method components
  var_comp <- VaR(R, method="modified", portfolio_method="component", weights=w)
  expect_true(is.list(var_comp))
  
  var_comp_hist <- VaR(R, method="historical", portfolio_method="component", weights=w)
  expect_true(is.list(var_comp_hist))
  
  var_comp_gaus <- VaR(R, method="gaussian", portfolio_method="component", weights=w)
  expect_true(is.list(var_comp_gaus))
  
  # Invert=FALSE
  expect_true(is.numeric(VaR(R[,1], invert=FALSE)))
})
