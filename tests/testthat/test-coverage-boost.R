library(PerformanceAnalytics)

test_that("SE computation works in VaR, ES, and SharpeRatio", {
  skip_on_cran()
  if (requireNamespace("RPESE", quietly = TRUE)) {
    data(edhec)
    R <- edhec[1:50, 1]
    
    # ES
    res_es <- ES(R, method="historical", SE=TRUE, invert=FALSE)
    expect_true(is.matrix(res_es) && nrow(res_es) > 1)
    
    # VaR
    res_var <- VaR(R, method="historical", SE=TRUE, invert=FALSE)
    expect_true(is.matrix(res_var) && nrow(res_var) > 1)
    
    # SharpeRatio SE
    res_sr <- SharpeRatio(R, FUN="StdDev", SE=TRUE)
    expect_true(is.matrix(res_sr) && nrow(res_sr) > 1)
    res_sr_var <- SharpeRatio(R, FUN="VaR", method="historical", SE=TRUE)
    expect_true(is.matrix(res_sr_var) && nrow(res_sr_var) > 1)
    res_sr_es <- SharpeRatio(R, FUN="ES", method="historical", SE=TRUE)
    expect_true(is.matrix(res_sr_es) && nrow(res_sr_es) > 1)
  }
})

test_that("SharpeRatio annualize and weights parameters", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  
  # annualize = TRUE
  res_ann <- SharpeRatio(R, FUN="StdDev", annualize=TRUE)
  expect_true(is.matrix(res_ann) || is.data.frame(res_ann) || is.numeric(res_ann))
  
  # weights
  res_w <- SharpeRatio(R, FUN="StdDev", weights=w)
  expect_true(is.matrix(res_w) || is.data.frame(res_w) || is.numeric(res_w))
})

test_that("VaR and ES multiple parameters coverage", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  # Testing locScaleRob
  expect_true(is.numeric(VaR(R[,1], clean="locScaleRob")))
  expect_true(is.numeric(ES(R[,1], clean="locScaleRob")))
  
  # VaR/ES historical with multiple columns
  res_var_hist <- VaR(R, method="historical")
  expect_true(is.matrix(res_var_hist) || is.data.frame(res_var_hist) || is.numeric(res_var_hist))
  
  res_es_hist <- ES(R, method="historical")
  expect_true(is.matrix(res_es_hist) || is.data.frame(res_es_hist) || is.numeric(res_es_hist))
  
  # SE Warnings and fallbacks (using invert=FALSE so we don't trigger the invert warning)
  expect_warning(VaR(R[,1], method="gaussian", SE=TRUE, invert=FALSE), "To return SEs, \"method\" must be \"historical\"")
  expect_warning(VaR(R[,1], method="historical", clean="boudt", SE=TRUE, invert=FALSE), "To return SEs, \"clean\" must be one of \"locScaleRob\" or \"none\"")
  expect_warning(VaR(R, method="historical", portfolio_method="component", weights=rep(0.25, 4), SE=TRUE, invert=FALSE), "To return SEs, \"portfolio_method\" must be \"single\"")
  
  expect_warning(ES(R[,1], method="gaussian", SE=TRUE, invert=FALSE), "To return SEs, \"method\" must be \"historical\"")
})
