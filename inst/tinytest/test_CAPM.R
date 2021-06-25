# Set seed
set.seed(0)

# Default test objects / Mock Data Objects
data(managers, package = "PerformanceAnalytics")

Ra = managers[,1,drop=FALSE]
Rb = managers[,8,drop=FALSE]
Rf = managers[,10,drop=FALSE]

xRa = PerformanceAnalytics::Return.excess(Ra, Rf)
xRb = PerformanceAnalytics::Return.excess(Rb, Rf)

# Test the coefficients API

# 1. Ordinary Least Squares
model.lm = PerformanceAnalytics::CAPM.coefficients(xRa, xRb)
expect_equal(model.lm$intercept,0.005774729)
expect_equal(model.lm$beta,0.3900712)

# 2. Robust Estimators (from RobStatTM)
model.lmRob.mOpt = PerformanceAnalytics::CAPM.coefficients(xRa, xRb, method="Rob", family="mopt")
expect_equal(model.lmRob.mOpt$intercept,0.006108399)
expect_equal(model.lmRob.mOpt$beta,0.3314739)

model.lmRob.opt = PerformanceAnalytics::CAPM.coefficients(xRa, xRb, method="Rob", family="opt")
expect_equal(model.lmRob.opt$intercept, 0.006104901)
expect_equal(model.lmRob.opt$beta,0.3317913)

model.lmRob.bisq = PerformanceAnalytics::CAPM.coefficients(xRa, xRb, method="Rob", family="bisquare")
expect_equal(model.lmRob.bisq$intercept,0.005452889)
expect_equal(model.lmRob.bisq$beta,0.364639)

# Tests for backward compatibility of CAPM lm() based functions

# 1. Functions inside CAPM.beta.R 
model.lm.beta = PerformanceAnalytics::CAPM.beta(Ra, Rb, Rf)
model.lm.beta.bear = PerformanceAnalytics::CAPM.beta.bear(Ra, Rb, Rf)
model.lm.beta.bull = PerformanceAnalytics::CAPM.beta.bull(Ra, Rb, Rf)
model.lm.timingRatio = PerformanceAnalytics::TimingRatio(Ra,Rb,Rf)

expect_equal(model.lm.beta,0.3900712)
expect_equal(model.lm.beta.bear,0.4264215)
expect_equal(model.lm.beta.bull,0.3005461)
expect_equal(model.lm.timingRatio,0.7048099)

# 2. Functions inside CAPM.alpha.R 
model.lm.alpha = PerformanceAnalytics::CAPM.beta(Ra, Rb, Rf)
expect_equal(model.lm.alpha,0.005774729)
