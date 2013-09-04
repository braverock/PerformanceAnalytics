library("sandwich")
library("lmtest")
library("strucchange")
data("PublicSchools")
ps <- na.omit(PublicSchools)
ps$Income <- ps$Income * 0.0001
fm.ps <- lm(Expenditure ~ Income + I(Income^3), data = ps)
sqrt(diag(vcov(fm.ps)))
sqrt(diag(vcovHC(fm.ps, type = "const")))
sqrt(diag(vcovHC(fm.ps, type = "HC0")))
sqrt(diag(vcovHC(fm.ps, type = "HC3")))
sqrt(diag(vcovHC(fm.ps, type = "HC4")))
coeftest(fm.ps, df = Inf, vcov = vcovHC(fm.ps, type = "HC0"))
coeftest(fm.ps, df = Inf, vcov = vcovHC(fm.ps, type = "HC4"))
plot(Expenditure ~ Income, data = ps,
     xlab = "per capita income",
     ylab = "per capita spending on public schools")
inc <- seq(0.5, 1.2, by = 0.001)
lines(inc, predict(fm.ps, data.frame(Income = inc)), col = 4, lty = 2)
fm.ps2 <- lm(Expenditure ~ Income, data = ps)
abline(fm.ps2, col = 4)
text(ps[2,2], ps[2,1], rownames(ps)[2], pos = 2)
## Willam H. Greene, Econometric Analysis, 2nd Ed.
## Chapter 14
## load data set, p. 385, Table 14.1
data(PublicSchools)

## omit NA in Wisconsin and scale income
ps <- na.omit(PublicSchools)
ps$Income <- ps$Income * 0.0001

## fit quadratic regression, p. 385, Table 14.2
fmq <- lm(Expenditure ~ Income + I(Income^2), data = ps)
summary(fmq)

## compare standard and HC0 standard errors
## p. 391, Table 14.3
library(sandwich)
coef(fmq)
sqrt(diag(vcovHC(fmq, type = "const")))
sqrt(diag(vcovHC(fmq, type = "HC0")))

if(require(lmtest)) {
  ## compare t ratio
  coeftest(fmq, vcov = vcovHC(fmq, type = "HC0"))
  
  ## White test, p. 393, Example 14.5
  wt <- lm(residuals(fmq)^2 ~ poly(Income, 4), data = ps)
  wt.stat <- summary(wt)$r.squared * nrow(ps)
  c(wt.stat, pchisq(wt.stat, df = 3, lower = FALSE))
  
  ## Bresch-Pagan test, p. 395, Example 14.7
  bptest(fmq, studentize = FALSE)
  bptest(fmq)
  
  ## Francisco Cribari-Neto, Asymptotic Inference, CSDA 45
  ## quasi z-tests, p. 229, Table 8
  ## with Alaska
  coeftest(fmq, df = Inf)[3,4]
  coeftest(fmq, df = Inf, vcov = vcovHC(fmq, type = "HC0"))[3,4]
  coeftest(fmq, df = Inf, vcov = vcovHC(fmq, type = "HC3"))[3,4]
  coeftest(fmq, df = Inf, vcov = vcovHC(fmq, type = "HC4"))[3,4]
  ## without Alaska (observation 2)
  fmq1 <- lm(Expenditure ~ Income + I(Income^2), data = ps[-2,])
  coeftest(fmq1, df = Inf)[3,4]
  coeftest(fmq1, df = Inf, vcov = vcovHC(fmq1, type = "HC0"))[3,4]
  coeftest(fmq1, df = Inf, vcov = vcovHC(fmq1, type = "HC3"))[3,4]
  coeftest(fmq1, df = Inf, vcov = vcovHC(fmq1, type = "HC4"))[3,4]
}

## visualization, p. 230, Figure 1
plot(Expenditure ~ Income, data = ps,
     xlab = "per capita income",
     ylab = "per capita spending on public schools")
inc <- seq(0.5, 1.2, by = 0.001)
lines(inc, predict(fmq, data.frame(Income = inc)), col = 4)
fml <- lm(Expenditure ~ Income, data = ps)
abline(fml)
text(ps[2,2], ps[2,1], rownames(ps)[2], pos = 2)