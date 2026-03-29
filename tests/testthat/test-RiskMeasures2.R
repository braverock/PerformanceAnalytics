library(PerformanceAnalytics)

test_that("VaR and ES variations work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  w <- rep(0.25, 4)
  
  # Clean methods
  expect_true(is.numeric(VaR(R[,1], clean="boudt")))
  expect_true(is.numeric(VaR(R[,1], clean="geltner")))
  expect_true(is.numeric(ES(R[,1], clean="boudt")))
  expect_true(is.numeric(ES(R[,1], clean="geltner")))
  
  # Marginal portfolio method (returns data.frame)
  expect_true(is.data.frame(VaR(R, portfolio_method="marginal", weights=w)))
  
  # Multiple p values
  expect_true(is.numeric(VaR(R[,1], p=0.95)))
  expect_true(is.numeric(VaR(R[,1], p=0.99)))
  
  # Passing moments directly
  mu <- colMeans(R)
  sigma <- cov(R)
  m3 <- M3.MM(R)
  m4 <- M4.MM(R)
  expect_true(is.numeric(VaR(R, method="modified", portfolio_method="single", mu=mu, sigma=sigma, m3=m3, m4=m4)))
  expect_true(is.numeric(ES(R, method="modified", portfolio_method="single", mu=mu, sigma=sigma, m3=m3, m4=m4)))
})

test_that("Drawdown methods work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1]
  
  expect_true(is.numeric(maxDrawdown(R)))
  expect_warning(res <- table.Drawdowns(R))
  expect_true(is.data.frame(res))
  expect_true(is.xts(Drawdowns(R)))
  expect_true(is.list(findDrawdowns(R)))
  
  # With weights
  w <- rep(0.25, 4)
  expect_true(is.numeric(maxDrawdown(edhec[1:50, 1:4], weights=w)))
})

test_that("Various Ratios work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:2]
  
  expect_true(is.numeric(SortinoRatio(R)))
  expect_true(is.numeric(SortinoRatio(R, MAR=0.01)))
  
  expect_true(is.numeric(DownsideSharpeRatio(R)))
  expect_true(is.numeric(UpsidePotentialRatio(R)))
  expect_true(is.numeric(Kappa(R, MAR=0, l=2)))
  expect_true(is.numeric(Kappa(R, MAR=0, l=3)))
  
  expect_true(is.numeric(AdjustedSharpeRatio(R)))
  expect_true(is.numeric(BernardoLedoitRatio(R)))
  expect_true(is.numeric(BurkeRatio(R, modified=TRUE)))
  expect_true(is.numeric(CalmarRatio(R)))
  expect_true(is.numeric(DRatio(R)))
  expect_true(is.numeric(InformationRatio(R[,1], R[,2])))
  expect_true(is.numeric(KellyRatio(R)))
  expect_true(is.numeric(MartinRatio(R)))
  expect_true(is.numeric(Modigliani(R[,1], R[,2])))
  expect_true(is.numeric(Omega(R)))
  expect_true(is.numeric(OmegaSharpeRatio(R)))
  expect_true(is.numeric(PainRatio(R)))
  expect_true(is.numeric(PainIndex(R)))
  expect_true(is.numeric(ProspectRatio(R, MAR=0)))
  expect_true(is.numeric(RachevRatio(R)))
  expect_true(is.numeric(SkewnessKurtosisRatio(R)))
  expect_true(is.numeric(TreynorRatio(R[,1], R[,2])))
  expect_true(is.numeric(UpDownRatios(R[,1], R[,2])))
  expect_true(is.numeric(VolatilitySkewness(R)))
})

test_that("Return.calculate variations", {
  skip_on_cran()
  data(prices)
  p <- prices[1:50, 1]
  
  # Return.calculate on single zoo returns zoo
  expect_s3_class(Return.calculate(p, method="discrete"), "zoo")
  expect_s3_class(Return.calculate(p, method="log"), "zoo")
  expect_s3_class(Return.calculate(p, method="difference"), "zoo")
})
