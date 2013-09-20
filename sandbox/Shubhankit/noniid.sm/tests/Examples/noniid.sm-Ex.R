pkgname <- "noniid.sm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('noniid.sm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ACStdDev.annualized")
### * ACStdDev.annualized

flush(stderr()); flush(stdout())

### Name: ACStdDev.annualized
### Title: Autocorrleation adjusted Standard Deviation
### Aliases: ACStdDev.annualized sd.annualized sd.multiperiod
###   StdDev.annualized
### Keywords: distribution models multivariate ts

### ** Examples

library(PerformanceAnalytics)
data(edhec)
ACStdDev.annualized(edhec,3)



cleanEx()
nameEx("CalmarRatio.Norm")
### * CalmarRatio.Norm

flush(stderr()); flush(stdout())

### Name: CalmarRatio.Norm
### Title: Normalized Calmar ratio
### Aliases: CalmarRatio.Norm
### Keywords: distribution models multivariate ts

### ** Examples

data(managers)
    CalmarRatio.Norm(managers[,1,drop=FALSE])
    CalmarRatio.Norm(managers[,1:6])



cleanEx()
nameEx("Cdrawdown")
### * Cdrawdown

flush(stderr()); flush(stdout())

### Name: CDrawdown
### Title: Chekhlov Conditional Drawdown at Risk
### Aliases: CDrawdown
### Keywords: Conditional Drawdown models

### ** Examples

library(PerformanceAnalytics)
data(edhec)
CDrawdown(edhec)



cleanEx()
nameEx("EMaxDDGBM")
### * EMaxDDGBM

flush(stderr()); flush(stdout())

### Name: EmaxDDGBM
### Title: Summary of Expected Drawdown using Brownian Motion Assumptions
###   and Return-Volatility
### Aliases: EmaxDDGBM
### Keywords: Assumptions Brownian Drawdown Expected models Motion Using

### ** Examples

library(PerformanceAnalytics)
data(edhec)
EmaxDDGBM(edhec)



cleanEx()
nameEx("GLMSmoothIndex")
### * GLMSmoothIndex

flush(stderr()); flush(stdout())

### Name: GLMSmoothIndex
### Title: GLM Index
### Aliases: GLMSmoothIndex Return.Geltner
### Keywords: distribution models multivariate non-iid ts

### ** Examples

require(PerformanceAnalytics)
 library(PerformanceAnalytics)
 data(edhec)
GLMSmoothIndex(edhec)



cleanEx()
nameEx("LoSharpe")
### * LoSharpe

flush(stderr()); flush(stdout())

### Name: LoSharpe
### Title: Andrew Lo Sharpe Ratio
### Aliases: LoSharpe
### Keywords: distribution models multivariate non-iid ts

### ** Examples

data(managers)
LoSharpe(managers,0,3)



cleanEx()
nameEx("Return.GLM")
### * Return.GLM

flush(stderr()); flush(stdout())

### Name: Return.GLM
### Title: GLM Return Model
### Aliases: Return.GLM
### Keywords: distribution model multivariate ts

### ** Examples

data(edhec)
Return.GLM(edhec,4)



cleanEx()
nameEx("Return.Okunev")
### * Return.Okunev

flush(stderr()); flush(stdout())

### Name: Return.Okunev
### Title: OW Return Model
### Aliases: Return.Okunev
### Keywords: distribution models multivariate ts

### ** Examples

data(managers)
head(Return.Okunev(managers[,1:3]),n=3)



cleanEx()
nameEx("SterlingRatio.Norm")
### * SterlingRatio.Norm

flush(stderr()); flush(stdout())

### Name: SterlingRatio.Norm
### Title: Normalized Sterling Ratio
### Aliases: SterlingRatio.Norm
### Keywords: distribution models multivariate ts

### ** Examples

data(managers)
    SterlingRatio.Norm(managers[,1,drop=FALSE])
    SterlingRatio.Norm(managers[,1:6])



cleanEx()
nameEx("UnSmoothReturn")
### * UnSmoothReturn

flush(stderr()); flush(stdout())

### Name: UnsmoothReturn
### Title: Unsmooth Time Series Return
### Aliases: UnsmoothReturn
### Keywords: models return smooth ts

### ** Examples

library(PerformanceAnalytics)
library(tseries)
data(managers)
UnsmoothReturn(managers,3)



cleanEx()
nameEx("chart.AcarSim")
### * chart.AcarSim

flush(stderr()); flush(stdout())

### Name: chart.AcarSim
### Title: Acar-Shane Maximum Loss Plot
### Aliases: chart.AcarSim
### Keywords: Drawdown Loss Maximum Simulated

### ** Examples

require(PerformanceAnalytics)
 library(PerformanceAnalytics)
 data(edhec)
chart.AcarSim(edhec)



cleanEx()
nameEx("chart.Autocorrelation")
### * chart.Autocorrelation

flush(stderr()); flush(stdout())

### Name: chart.Autocorrelation
### Title: Stacked Bar Autocorrelation Plot
### Aliases: chart.Autocorrelation
### Keywords: Autocorrelation factors lag

### ** Examples

data(edhec)
chart.Autocorrelation(edhec[,1])



cleanEx()
nameEx("se.LoSharpe")
### * se.LoSharpe

flush(stderr()); flush(stdout())

### Name: se.LoSharpe
### Title: Andrew Lo Sharpe Ratio Statistics
### Aliases: se.LoSharpe
### Keywords: distribution models multivariate non-iid ts

### ** Examples

data(managers)
se.LoSharpe(managers,0,3)



cleanEx()
nameEx("table.ComparitiveReturn.GLM")
### * table.ComparitiveReturn.GLM

flush(stderr()); flush(stdout())

### Name: table.ComparitiveReturn.GLM
### Title: Compenent Decomposition of Table of Unsmooth Returns for GLM
###   Model
### Aliases: table.ComparitiveReturn.GLM
### Keywords: GLM models return ts unsmooth

### ** Examples

library(PerformanceAnalytics)
library(tseries)
data(managers)
table.ComparitiveReturn.GLM(managers,3)



cleanEx()
nameEx("table.EMaxDDGBM")
### * table.EMaxDDGBM

flush(stderr()); flush(stdout())

### Name: table.EMaxDDGBM
### Title: Summary of Expected Drawdown using Brownian Motion Assumptions
###   and Return-Volatility
### Aliases: table.EMaxDDGBM
### Keywords: Assumptions Brownian Drawdown Expected models Motion Using

### ** Examples

library(PerformanceAnalytics)
data(edhec)
table.EMaxDDGBM(edhec)



cleanEx()
nameEx("table.Sharpe")
### * table.Sharpe

flush(stderr()); flush(stdout())

### Name: table.Sharpe
### Title: Sharpe Ratio Statistics Summary
### Aliases: table.Sharpe
### Keywords: distribution models multivariate non-iid ts

### ** Examples

data(managers)
table.Sharpe(managers,0,3)



cleanEx()
nameEx("table.UnsmoothReturn")
### * table.UnsmoothReturn

flush(stderr()); flush(stdout())

### Name: table.UnsmoothReturn
### Title: Table of Unsmooth Returns
### Aliases: table.UnsmoothReturn
### Keywords: models return smooth ts

### ** Examples

library(PerformanceAnalytics)
library(tseries)
data(managers)
table.UnsmoothReturn(managers,3)



cleanEx()
nameEx("table.normDD")
### * table.normDD

flush(stderr()); flush(stdout())

### Name: table.normDD
### Title: Generalised Lambda Distribution Drawdown
### Aliases: table.normDD
### Keywords: Assumptions Brownian Drawdown Motion Simulated Using

### ** Examples

library(PerformanceAnalytics)
data(edhec)
table.normDD(edhec[1:30,1])



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
