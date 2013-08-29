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
ACStdDev.annualized(edhec,3)



cleanEx()
nameEx("AcarSim")
### * AcarSim

flush(stderr()); flush(stdout())

### Name: AcarSim
### Title: Acar-Shane Maximum Loss Plot
### Aliases: AcarSim
### Keywords: Drawdown Loss Maximum Simulated

### ** Examples

library(PerformanceAnalytics)
AcarSim()



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
nameEx("GLMSmoothIndex")
### * GLMSmoothIndex

flush(stderr()); flush(stdout())

### Name: GLMSmoothIndex
### Title: GLM Index
### Aliases: GLMSmoothIndex Return.Geltner
### Keywords: distribution models multivariate non-iid ts

### ** Examples

data(edhec)
head(GLMSmoothIndex(edhec))



cleanEx()
nameEx("LoSharpe")
### * LoSharpe

flush(stderr()); flush(stdout())

### Name: LoSharpe
### Title: Andrew Lo Sharpe Ratio
### Aliases: LoSharpe
### Keywords: distribution models multivariate non-iid ts

### ** Examples

data(edhec)
head(LoSharpe(edhec,0,3)



cleanEx()
nameEx("Return.GLM")
### * Return.GLM

flush(stderr()); flush(stdout())

### Name: Return.GLM
### Title: GLM Return Model
### Aliases: Return.GLM
### Keywords: distribution model multivariate ts

### ** Examples

library(PerformanceAnalytics)
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
nameEx("chart.AcarSim")
### * chart.AcarSim

flush(stderr()); flush(stdout())

### Name: chart.AcarSim
### Title: Acar-Shane Maximum Loss Plot
### Aliases: chart.AcarSim
### Keywords: Drawdown Loss Maximum Simulated

### ** Examples

library(PerformanceAnalytics)
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

data(edhec[,1])
chart.Autocorrelation(edhec[,1])



cleanEx()
nameEx("noniid.sm-package")
### * noniid.sm-package

flush(stderr()); flush(stdout())

### Name: noniid.sm-package
### Title: What the package does (short line) ~~ package title ~~
### Aliases: noniid.sm-package noniid.sm
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



cleanEx()
nameEx("se.LoSharpe")
### * se.LoSharpe

flush(stderr()); flush(stdout())

### Name: se.LoSharpe
### Title: Andrew Lo Sharpe Ratio Statistics
### Aliases: se.LoSharpe
### Keywords: distribution models multivariate non-iid ts

### ** Examples

data(edhec)
head(se.LoSharpe(edhec,0,3)



cleanEx()
nameEx("table.EmaxDDGBM")
### * table.EmaxDDGBM

flush(stderr()); flush(stdout())

### Name: table.EMaxDDGBM
### Title: Expected Drawdown using Brownian Motion Assumptions
### Aliases: table.EMaxDDGBM
### Keywords: Assumptions Brownian Drawdown Expected models Motion Using

### ** Examples

library(PerformanceAnalytics)
data(edhec)
table.EmaxDDGBM(edhec)



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
