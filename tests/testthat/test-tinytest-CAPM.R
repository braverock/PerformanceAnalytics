library(PerformanceAnalytics)

test_that("CAPM and SFM coefficients match legacy expectations", {
  # Set seed
  set.seed(0)
  
  # Default test objects / Mock Data Objects
  data(managers, package = "PerformanceAnalytics")
  
  Ra = managers[,1,drop=FALSE]
  Rb = managers[,8,drop=FALSE]
  Rf = managers[,10,drop=FALSE]
  
  xRa = PerformanceAnalytics::Return.excess(Ra, Rf)
  xRb = PerformanceAnalytics::Return.excess(Rb, Rf)
  
  # 1. Ordinary Least Squares
  model.lm = PerformanceAnalytics::SFM.coefficients(xRa, xRb, method="LS")
  expect_equal(as.numeric(model.lm[1]), 0.006, tolerance = 1e-3)
  expect_equal(as.numeric(model.lm[2]), 0.39, tolerance = 1e-2)
  
  if(requireNamespace("RobStatTM", quietly = TRUE)){
    # 2. Robust Estimators (from RobStatTM)
    model.lmRob.mOpt = PerformanceAnalytics::SFM.coefficients(xRa, xRb, method="Robust", family="mopt")
    expect_equal(as.numeric(model.lmRob.mOpt[1]), 0.006, tolerance = 1e-3)
    expect_equal(as.numeric(model.lmRob.mOpt[2]), 0.331, tolerance = 1e-3)
    
    model.lmRob.opt = PerformanceAnalytics::SFM.coefficients(xRa, xRb, method="Robust", family="opt")
    expect_equal(as.numeric(model.lmRob.opt[1]), 0.006, tolerance = 1e-3)
    expect_equal(as.numeric(model.lmRob.opt[2]), 0.332, tolerance = 1e-3)
    
    model.lmRob.bisq = PerformanceAnalytics::SFM.coefficients(xRa, xRb, method="Robust", family="bisquare")
    expect_equal(as.numeric(model.lmRob.bisq[1]), 0.005, tolerance = 1e-3)
    expect_equal(as.numeric(model.lmRob.bisq[2]), 0.365, tolerance = 1e-3)
  }
  
  # 1. Functions inside CAPM.beta.R 
  model.lm.beta = PerformanceAnalytics::CAPM.beta(Ra, Rb, Rf)
  model.lm.beta.bear = PerformanceAnalytics::CAPM.beta.bear(Ra, Rb, Rf)
  model.lm.beta.bull = PerformanceAnalytics::CAPM.beta.bull(Ra, Rb, Rf)
  model.lm.timingRatio = PerformanceAnalytics::TimingRatio(Ra,Rb,Rf)
  
  expect_equal(as.numeric(model.lm.beta), 0.390071248399483, tolerance = 1e-10)
  expect_equal(as.numeric(model.lm.beta.bear), 0.4264215, tolerance = 1e-7)
  expect_equal(as.numeric(model.lm.beta.bull), 0.300546080919799, tolerance = 1e-10)
  expect_equal(as.numeric(model.lm.timingRatio), 0.704809869142456, tolerance = 1e-10)
  
  # 2. Functions inside CAPM.alpha.R 
  model.lm.alpha = PerformanceAnalytics::CAPM.alpha(Ra, Rb, Rf)
  expect_equal(as.numeric(model.lm.alpha), 0.00577472877485089, tolerance = 1e-10)
})
