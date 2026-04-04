# This file includes following functions
# VaR.gpd, VaR.gpd.plots, VaR.norm, VaR.norm.plots, VaR.backtest,
# gpd, gpdf, gpdq, pdfVaReps, pdfESeps, gpd.lik 

"VaR.gpd" <-
function (ydat, p = 0.01, p.tr = 0.97, drift.appx = FALSE, init = c(1.00, 0.3), cflevel = 0.95)
{
    if (length(ydat[ydat < 0]) != 0) stop("Negative value in input data!")
    
    n <- length(ydat) - 1
    y <- vector(length = n)
    y <- 100 * (ydat[-1]/ydat[-(n + 1)] - 1)

    if (drift.appx) y.mean <- mean(y) else y.mean <- 0
    y.stdv <- sd(y)
    threshold <- qnorm(p.tr) * y.stdv
    
    x <- sort(-y)
    x.cut <- x[x > threshold]
    n.cut <- length(x.cut)
    nnu <- n/n.cut
    gpd.liklhd <- function(a){
	    gpd.lik(a,x.cut,threshold)
    }
    fit <- optim(init, gpd.liklhd, hessian = TRUE, method = "Nelder-Mead")
    
    y.VaR = gpd(fit$par, threshold, nnu*p) #* ydat[length(ydat)] /100.0
    VaR.LogLik <- function (R){
	   sum(log(pdfVaReps((x.cut-threshold), fit$par[2], R,  threshold, p*nnu)))
    }
    VaR.LogLikDiff <-function (R){
	    y <- VaR.LogLik(R)-VaR.LogLik(y.VaR)+0.5*qchisq(cflevel,df=1)
	    if (is.nan(y)) y <- -1000000
	    y
    }
    VaR.low <- uniroot(VaR.LogLikDiff, c(0.01,y.VaR))
    VaR.big <- uniroot(VaR.LogLikDiff, c(y.VaR,99.99))
        
    y.ES = (y.VaR+fit$par[1]-fit$par[2]*threshold)/(1-fit$par[2])
    ES.LogLik <- function (R){
	   sum(log(pdfESeps((x.cut-threshold), fit$par[2], R,  threshold, p*nnu)))
    }
    ES.LogLikDiff <-function (R){
	    y <- ES.LogLik(R)-ES.LogLik(y.ES)+0.5*qchisq(cflevel,df=1)
	    if (is.nan(y)) y <- -1000000
	    y
    }
    ES.low <- uniroot(ES.LogLikDiff, c(0.01,y.ES))
    ES.big <- uniroot(ES.LogLikDiff, c(y.ES,99.99))

    list(VaR = y.VaR, VaR.interval = c(VaR.low$root, VaR.big$root), ES = y.ES, ES.interval = c(ES.low$root, ES.big$root),
         data = y, cdata = x.cut, conf.level = p, tr = threshold, mean = y.mean, std = y.stdv, gfit = fit$par, int.conf.level = cflevel)
}

"VaR.gpd.plots" <-
function (z) 
{
    plot(c(1:length(z$data)), z$data, xlab = "i", ylab = "Daily Return (%)", main = "Daily Return", type = "l")
    abline(h = z$VaR, col = 4)
    abline(h = -z$VaR, col = 4)
	
    n.cut <- length(z$cdata)
    t.new <- 1 - c((n.cut - 1):0)/n.cut
    plot(z$cdata, t.new, xlab = "Observed Value", ylab = "", main = "Sample Distrubution")
    zz <- seq(z$tr, 3 * max(z$cdata), length = 300)
    q <- gpdf(z$gfit, z$tr, zz)
    lines(zz, q, col = 4)

    plot(gpdq(z$gfit, z$tr, 1 - (1:n.cut)/(n.cut + 1)), z$cdata, 
        ylab = "Empirical", xlab = "Model", main = "Quantile Plot")
    abline(0, 1, col = 4)


    VaR.loglik <-function (z, R){
      sum(log(pdfVaReps((z$cdata-z$tr), z$gfit[2], R,  z$tr, z$conf.level*length(z$data)/length(z$cdata))))
    }
    VaR.loglik.diff <-function (R){
	    VaR.loglik(z, varVaR[i])-VaR.loglik(z, z$VaR)
    }
    varVaR <- seq(z$VaR.interval[1]*0.9,z$VaR.interval[1]*1.5, 0.001)  
    LogLik <- vector(length = length(varVaR))
    for (i in 1:length(varVaR)) LogLik[i] <- VaR.loglik.diff(varVaR[i]) 
    plot(varVaR, LogLik, xlab = "VaR", ylab = "Log Likelihood", type = "l")
    abline(h = -0.5*qchisq(z$int.conf.level,df=1), col = 4)
    
    
    ES.loglik <- function (R){
	   sum(log(pdfESeps((z$cdata-z$tr), z$gfit[2], R,  z$tr,z$conf.level*length(z$data)/length(z$cdata))))
    }
    ES.loglik.diff <-function (R){
	    y <- ES.loglik(R)-ES.loglik(z$ES)
	    if (is.nan(y)) y <- -1000000
	    y
    }
    varES <- seq(z$ES.interval[1]*0.9,z$ES.interval[1]*1.5, 0.001)  
    LogLik <- vector(length = length(varES))
    for (i in 1:length(varES)) LogLik[i] <- ES.loglik.diff(varES[i]) 
    plot(varES, LogLik, xlab = "ES", ylab = "Log Likelihood", type = "l")
    abline(h = -0.5*qchisq(z$int.conf.level,df=1), col = 4)
    
    
}

"VaR.norm" <-
function (ydat, p = 0.99, dt = 1, type = "long", drift.appx = FALSE, lin.appx = TRUE) 
{

    if (length(ydat[ydat < 0]) != 0) stop("Negative value in parameter 'ydat'")
    n <- length(ydat) - 1
    y <- vector(length = n)
    y <- log(ydat[-1]/ydat[-(n + 1)])

    if (drift.appx) y.mean <- mean(y) else y.mean <- 0
    y.stdv <- sd(y)
    
    if (lin.appx) {
        if (type == "long") 
            y.VaR = ydat[n] * (-y.mean * dt - sqrt(dt) * y.stdv * qnorm(1 - p))
        else y.VaR = -ydat[n] * (-y.mean * dt + sqrt(dt) * y.stdv * qnorm(1 - p))
    }
    else {
        if (type == "long") y.VaR = ydat[n] * (1 - exp(y.mean * dt + sqrt(dt) * y.stdv * qnorm(1 - p)))
        else y.VaR = -ydat[n] * (1 - exp(y.mean * dt - sqrt(dt) * y.stdv * qnorm(1 - p)))
    }
    list(VaR = y.VaR, data = ydat, cdata = y, liq.period = dt, type.VAR = type, conf.level = p, mean = y.mean, std = y.stdv)
}

"VaR.norm.plots" <-
function (z) 
{ 
    plot(c(1:length(z$cdata)), z$cdata, xlab = "i", ylab = "Daily Log Return", main = "Daily Log Return")
    abline(h = log(1+z$VaR/z$data[length(z$data)]), col = 4)
    abline(h = log(1-z$VaR/z$data[length(z$data)]), col = 4)
    
    histo <- hist(z$cdata, breaks = 100, probability = TRUE, xlab = "Daily Log Return", ylab = "Events", main = "Histogram of Daily Return" )
    lines(histo$breaks, dnorm(histo$breaks, mean = z$mean, sd = z$std), col = 4)
}

"VaR.backtest" <-
function(x, VaR, p){
  n <- length(x)
  x.cut <- x[x>VaR]
  n1 <- length(x.cut)
  x <- prop.test(n1, n, p)
  x$p.value
}

"gpd" <-
function (a, u, p) 
{
    u + (a[1]/a[2]) * ((p)^(-a[2]) - 1)
}

"gpdf" <-
function (a, u, z) 
{
    1 - (1 + (a[2] * (z - u))/a[1])^(-1/a[2])
}

"gpdq" <-
function (a, u, p) u + (a[1] * (p^(-a[2]) - 1))/a[2]


"pdfVaReps" <- function(y, eps,  VaR,  u, p)
{
    if (eps == 0.00) {
       out <- -p*exp(y/(VaR-u))/(VaR-u)
    } else {
       out1 <- (-1.0+(p)^(-eps))/(eps*(VaR-u))
       out2 <- (1.0+out1*eps*y)^(-1.0/eps-1.0)
       out <- out1*out2
    }
    out
}

"pdfESeps" <- function(y, eps,  ES,  u, p) {
   out1 <- (eps + p^(-eps)-1)/(1-eps)/(ES-u)
   out  <- (out1/eps)*((1+out1*y)^(-1/eps-1))
   out
}

"gpd.lik" <- function(a,x,u) {
    n <- length(x)
    y <- (x - u)/a[1]
    if (a[2] == 0) {
        ret = n * log(a[1]) + sum(y)
    }
    else {
        y <- 1 + a[2] * y
        ret <- n * log(a[1]) + sum(log(y)) * (1 + 1/a[2])
    }
    ret
}


#source("DJIA.R")
#attach(DJIA)

#source("exchangerates.R")
#attach(exchange.rates)

#package.skeleton(list =
#c("exchange.rates","DJIA","VaR.gpd","VaR.gpd.plots","VaR.norm",
#"VaR.norm.plots", "VaR.backtest.prop.test"), name = "VaR")
