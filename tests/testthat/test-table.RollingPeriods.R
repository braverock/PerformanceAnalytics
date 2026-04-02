library(PerformanceAnalytics)
library(xts)

test_that("table.TrailingPeriodsRel works securely with Rb and multiple FUNCS", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:2]
  Rb <- edhec[1:50, 3, drop=FALSE]
  
  res <- table.TrailingPeriodsRel(R, Rb, FUNCS=c("cor", "CAPM.beta"), 
                                  funcs.names=c("Cor", "Beta"), periods=12)
  
  expect_true("Last 12 month Cor to Distressed Securities" %in% rownames(res))
  expect_true("Last 12 month Beta to Distressed Securities" %in% rownames(res))
  
  # Manual check
  tail_R <- tail(R[,1], 12)
  tail_Rb <- tail(Rb[,1], 12)
  
  man_cor <- cor(tail_R, tail_Rb)
  man_beta <- CAPM.beta(tail_R, tail_Rb)
  
  expect_equal(res["Last 12 month Cor to Distressed Securities", 1], round(as.numeric(man_cor), 4))
  expect_equal(res["Last 12 month Beta to Distressed Securities", 1], round(as.numeric(man_beta), 4))
})

test_that("table.RollingPeriods and table.TrailingPeriods are aliases", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1, drop=FALSE]
  
  res_roll <- table.RollingPeriods(R, periods=12, FUNCS="mean", funcs.names="Mean")
  res_trail <- table.TrailingPeriods(R, periods=12, FUNCS="mean", funcs.names="Mean")
  
  expect_equal(res_roll, res_trail)
})

