## expectedShortFallFunctions.r
## author: Eric Zivot
## created: April 29, 2009
## update history: 
## December 2, 2009
##    Fixed bootstrapIncrementalES to work with one asset portfolio
##
## purpose: functions for computing expected shortfall
##
## Functions:
## normalES
## normalPortfolioES
## normalMarginalES           Not done
## normalComponentES          Not done
## normalIncrementalES
## normalESreport             Not done
## modifiedES                 Not done
## modifiedPortfolioES        Not done  
## modifiedMarginalES         Not done
## modifiedComponentES        Not done
## modifiedIncrementalES      Not done
## modifiedESreport           Not done
## bootstrapES
## bootstrapPortfolioES
## bootstrapIncrementalES
## bootstrapMarginalES
## bootstrapComponentES
## bootstrapESreport
##
## References:
## 1. Dowd, K. (2002). Measuring Market Risk, John Wiley and Sons
## 2. Boudt, Peterson and Croux (2008), "Estimation and Decomposition of Downside
##    Risk for Portfolios with Non-Normal Returns," Journal of Risk.
## 3. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and 
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
##    Bank of Japan.

##
## 1. ES functions based on the normal distribution
##

normalES <- function(mu, sigma, tail.prob = 0.01) {
## compute normal ES for collection of assets given mean and sd vector
## inputs:
## mu       n x 1 vector of expected returns
## sigma    n x 1 vector of standard deviations
## tail.prob  scalar tail probability
## output:
## ES      n x 1 vector of left tail average returns reported as a positive number
  mu = as.matrix(mu)
  sigma = as.matrix(sigma)
  if ( nrow(mu) != nrow(sigma) )
    stop("mu and sigma must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")



#' calculates Expected Shortfall(ES) (or Conditional Value-at-Risk(CVaR) for
#' univariate and component, using a variety of analytical methods.
#' 
#' Calculates Expected Shortfall(ES) (also known as) Conditional Value at
#' Risk(CVaR) for univariate, component, and marginal cases using a variety of
#' analytical methods.
#' 
#' 
#' @aliases ES CVaR ETL
#' @param R a vector, matrix, data frame, timeSeries or zoo object of asset
#' returns
#' @param p confidence level for calculation, default p=.95
#' @param method one of "modified","gaussian","historical", "kernel", see
#' Details.
#' @param clean method for data cleaning through \code{\link{Return.clean}}.
#' Current options are "none", "boudt", or "geltner".
#' @param portfolio_method one of "single","component","marginal" defining
#' whether to do univariate, component, or marginal calc, see Details.
#' @param weights portfolio weighting vector, default NULL, see Details
#' @param mu If univariate, mu is the mean of the series. Otherwise mu is the
#' vector of means of the return series , default NULL, , see Details
#' @param sigma If univariate, sigma is the variance of the series. Otherwise
#' sigma is the covariance matrix of the return series , default NULL, see
#' Details
#' @param m3 If univariate, m3 is the skewness of the series. Otherwise m3 is
#' the coskewness matrix of the returns series, default NULL, see Details
#' @param m4 If univariate, m4 is the excess kurtosis of the series. Otherwise
#' m4 is the cokurtosis matrix of the return series, default NULL, see Details
#' @param invert TRUE/FALSE whether to invert the VaR measure.  see Details.
#' @param operational TRUE/FALSE, default TRUE, see Details.
#' @param \dots any other passthru parameters
#' @note The option to \code{invert} the ES measure should appease both
#' academics and practitioners.  The mathematical definition of ES as the
#' negative value of extreme losses will (usually) produce a positive number.
#' Practitioners will argue that ES denotes a loss, and should be internally
#' consistent with the quantile (a negative number).  For tables and charts,
#' different preferences may apply for clarity and compactness.  As such, we
#' provide the option, and set the default to TRUE to keep the return
#' consistent with prior versions of PerformanceAnalytics, but make no value
#' judgement on which approach is preferable.
#' @section Background: This function provides several estimation methods for
#' the Expected Shortfall (ES) (also called Expected Tail Loss (ETL)
#' orConditional Value at Risk (CVaR)) of a return series and the Component ES
#' (ETL/CVaR) of a portfolio.
#' 
#' At a preset probability level denoted \eqn{c}, which typically is between 1
#' and 5 per cent, the ES of a return series is the negative value of the
#' expected value of the return when the return is less than its
#' \eqn{c}-quantile.  Unlike value-at-risk, conditional value-at-risk has all
#' the properties a risk measure should have to be coherent and is a convex
#' function of the portfolio weights (Pflug, 2000).  With a sufficiently large
#' data set, you may choose to estimate ES with the sample average of all
#' returns that are below the \eqn{c} empirical quantile. More efficient
#' estimates of VaR are obtained if a (correct) assumption is made on the
#' return distribution, such as the normal distribution. If your return series
#' is skewed and/or has excess kurtosis, Cornish-Fisher estimates of ES can be
#' more appropriate. For the ES of a portfolio, it is also of interest to
#' decompose total portfolio ES into the risk contributions of each of the
#' portfolio components. For the above mentioned ES estimators, such a
#' decomposition is possible in a financially meaningful way.
#' @author Brian G. Peterson and Kris Boudt
#' @seealso \code{\link{VaR}} \cr \code{\link{SharpeRatio.modified}} \cr
#' \code{\link{chart.VaRSensitivity}} \cr \code{\link{Return.clean}}
#' @references Boudt, Kris, Peterson, Brian, and Christophe Croux. 2008.
#' Estimation and decomposition of downside risk for portfolios with non-normal
#' returns. 2008. The Journal of Risk, vol. 11, 79-103.
#' 
#' Cont, Rama, Deguest, Romain and Giacomo Scandolo. Robustness and sensitivity
#' analysis of risk measurement procedures. Financial Engineering Report No.
#' 2007-06, Columbia University Center for Financial Engineering.
#' 
#' Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk
#' Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002,
#' v 5.
#' 
#' Martellini, Lionel, and Volker Ziemann.  Improved Forecasts of Higher-Order
#' Comoments and Implications for Portfolio Selection. 2007. EDHEC Risk and
#' Asset Management Research Centre working paper.
#' 
#' Pflug, G. Ch.  Some remarks on the value-at-risk and the conditional
#' value-at-risk. In S. Uryasev, ed., Probabilistic Constrained Optimization:
#' Methodology and Applications, Dordrecht: Kluwer, 2000, 272-281.
#' 
#' Scaillet, Olivier. Nonparametric estimation and sensitivity analysis of
#' expected shortfall. Mathematical Finance, 2002, vol. 14, 74-86.
#' @keywords ts multivariate distribution models
#' @examples
#' 
#'     data(edhec)
#' 
#'     # first do normal ES calc
#'     ES(edhec, p=.95, method="historical")
#' 
#'     # now use Gaussian
#'     ES(edhec, p=.95, method="gaussian")
#' 
#'     # now use modified Cornish Fisher calc to take non-normal distribution into account
#'     ES(edhec, p=.95, method="modified")
#' 
#'     # now use p=.99
#'     ES(edhec, p=.99)
#'     # or the equivalent alpha=.01
#'     ES(edhec, p=.01)
#' 
#'     # now with outliers squished
#'     ES(edhec, clean="boudt")
#' 
#'     # add Component ES for the equal weighted portfolio
#'     ES(edhec, clean="boudt", portfolio_method="component")
#' 
  ES = mu - sigma*dnorm(qnorm(tail.prob))/tail.prob
  return(-ES)
}

normalPortfolioES <- function(mu, Sigma, w, tail.prob = 0.01) {
## compute normal portfolio ES given portfolio weights, mean vector and
## covariance matrix
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## pES      scalar left tail average return of normal portfolio distn returned
##           as a positive number.
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  pES = crossprod(w, mu) + sqrt( t(w) %*% Sigma %*% w )*dnorm(qnorm(tail.prob))/tail.prob
  return(as.numeric(pES))
}

normalIncrementalES <- function(mu, Sigma, w, tail.prob = 0.01) {
## compute normal incremental ES given portfolio weights, mean vector and
## covariance matrix
## Incremental ES is defined as the change in portfolio ES that occurs
## when an asset is removed from the portfolio
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## iES      n x 1 vector of incremental ES values
## References:
## Jorian, P. (2007). Value at Risk,  pg. 168.
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  
  n.w = nrow(mu)
  ## portfolio ES with all assets
  pES = normalPortfolioES(mu, Sigma, w, tail.prob)
  temp.w = w
  iES = matrix(0, n.w, 1)
  for (i in 1:n.w) {
  ## set weight for asset i to zero and renormalize
    temp.w[i,1] = 0
    temp.w = temp.w/sum(temp.w)
    pES.new = normalPortfolioES(mu, Sigma, temp.w, tail.prob)  
    iES[i,1] = pES.new - pES
  ## reset weight
    temp.w = w
  }
  return(iES)
}


##
## 2. ES functions based on simulated (bootstrapped) data
##

bootstrapES <- function(bootData, tail.prob = 0.01, 
                        method=c("HS", "CornishFisher")) {
## Compute ES given bootstrap data. ES is computed as the sample mean beyond
## VaR where VaR is computed either as the sample quantile or the quantile using 
## the Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of B bootstrap returns on n assets in portfolio
## tail.prob  scalar tail probability
## method     character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package
## output:
## ES        n x 1 matrix of ES values for each asset
## References:
  require(ff,quietly=TRUE)
  method = method[1]
  if (!is.ff(bootData))
    bootData = as.matrix(bootData)
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  n.assets = ncol(bootData) 
  ES = matrix(0, n.assets, 1)
  rownames(ES) = colnames(bootData)
  colnames(ES) = "ES"
  ## loop over all assets and compute ES
  for (i in 1:n.assets) {
    if (method == "HS") {
    ## use empirical quantile
      VaR = quantile(bootData[,i], probs=tail.prob)
    } else {
    ## use Cornish-Fisher quantile
      VaR = -VaR.CornishFisher(bootData[,i], p=(1-tail.prob))
    }  
    idx = which(bootData[,i] <= VaR)
    ES[i,1] = mean(bootData[idx, i])
  }  
return(-ES)
}

bootstrapPortfolioES <- function(bootData, w, tail.prob = 0.01, 
                                 method=c("HS", "CornishFisher")) {
## Compute portfolio ES given bootstrap data and portfolio weights. VaR is computed 
## either as the sample quantile or as an estimated quantile using the 
## Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of bootstrap returns on n assets in portfolio
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## method character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package
## output:
## pES        scalar, portfolio ES value
## References:
##    Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and 
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
##    Bank of Japan.

  require(ff, quietly=TRUE)
  method = method[1]
  if (!is.ff(bootData))
    bootData = as.matrix(bootData)
  w = as.matrix(w)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  if (is.ff(bootData)) {
  ## use on disk ff objects
    dummy<-TRUE; if(!dummy){i1=1;i2=2} #fake assign to make R CMD check happy
    r.p = ffrowapply( bootData[i1:i2, ]%*%w, X=bootData, RETURN=TRUE, RETCOL=1)
  } else {  
  ## use in RAM objects
    r.p = bootData %*% w  
  }
  if (method == "HS") {
  ## use empirical quantile
    pVaR = quantile(r.p[], prob=tail.prob)
    idx = which(r.p[] <= pVaR)
    pES = -mean(r.p[idx]) 
  } else {
  ## use Cornish-Fisher expansion
    pVaR = -VaR.CornishFisher(r.p[], p=(1-tail.prob))
    idx = which(r.p[] <= pVaR)
    pES = -mean(r.p[idx]) 
  }
return(pES)
}


bootstrapIncrementalES <- function(bootData, w, tail.prob = 0.01, 
                                   method=c("HS", "CornishFisher")) {
## Compute incremental ES given bootstrap data and portfolio weights.
## Incremental ES is defined as the change in portfolio ES that occurs
## when an asset is removed from the portfolio and allocation is spread equally
## among remaining assets. VaR used in ES computation is computed either as the 
## sample quantile or as an estimated quantile using the Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of B bootstrap returns on n assets in portfolio
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## method     character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package
## output:
## iES        n x 1 matrix of incremental VaR values for each asset
## References:
## Jorian, P. (2007). Value-at-Risk, pg. 168.
## 
  require(PerformanceAnalytics)
  require(ff)
  method = method[1]
  if (!is.ff(bootData))
    bootData = as.matrix(bootData)
  w = as.matrix(w)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  
  if (ncol(bootData) == 1) {
  ## EZ: Added 12/2/2009
  ## one asset portfolio so iES = 0 by construction
    iES = 0
  } else { 
      
  n.w = nrow(w)
  ## portfolio VaR with all assets
  if (is.ff(bootData)) {
  ## use on disk ff object
    dummy<-TRUE; if(!dummy){i1=1;i2=2} #fake assign to make R CMD check happy
    r.p = ffrowapply( bootData[i1:i2, ]%*%w, X=bootData, RETURN=TRUE, RETCOL=1)
  } else {
  ## use in RAM object
    r.p = bootData %*% w
  }
  if (method == "HS") {
  ## use empirical quantile to compute VaR
    pVaR = quantile(r.p[], prob=tail.prob)
    idx = which(r.p[] <= pVaR)
    pES = -mean(r.p[idx]) 
    temp.w = w
    iES = matrix(0, n.w, 1)
    rownames(iES) = colnames(bootData)
    colnames(iES) = "i.ES"
    for (i in 1:n.w) {
    ## set weight for asset i to zero and renormalize
      temp.w[i,1] = 0
      temp.w = temp.w/sum(temp.w)
      if (is.ff(bootData)) {
        dummy<-TRUE; if(!dummy){i1=1;i2=2} #fake assign to make R CMD check happy
        temp.r.p = ffrowapply( bootData[i1:i2, ]%*%temp.w, X=bootData, RETURN=TRUE, RETCOL=1)
      } else {
        temp.r.p = bootData %*% temp.w
      }
      pVaR.new = quantile(temp.r.p[], prob=tail.prob)    
      idx = which(temp.r.p[] <= pVaR.new)
      pES.new = -mean(temp.r.p[idx])
      iES[i,1] = pES.new - pES
    ## reset weight
      temp.w = w
  }
  } else {
  ## use Cornish-Fisher VaR
    pVaR = -VaR.CornishFisher(r.p[], p=(1-tail.prob))
    idx = which(r.p[] <= pVaR)
    pES = -mean(r.p[idx]) 
    temp.w = w
    iES = matrix(0, n.w, 1)
    rownames(iES) = colnames(bootData)
    colnames(iES) = "i.ES"
    for (i in 1:n.w) {
    ## set weight for asset i to zero and renormalize
      temp.w[i,1] = 0
      temp.w = temp.w/sum(temp.w)
      if (is.ff(bootData)) {
        dummy<-TRUE; if(!dummy){i1=1;i2=2} #fake assign to make R CMD check happy
        temp.r.p = ffrowapply( bootData[i1:i2, ]%*%temp.w, X=bootData, RETURN=TRUE, RETCOL=1)
      } else {
        temp.r.p = bootData %*% temp.w
      }  
      pVaR.new = -VaR.CornishFisher(temp.r.p[], p=(1-tail.prob))   
      idx = which(temp.r.p[] <= pVaR.new)
      pES.new = -mean(temp.r.p[idx])
      iES[i,1] = pES.new - pES
    ## reset weight
      temp.w = w
  }
  }
  }
  return(iES)
}

bootstrapMarginalES <- function(bootData, w, delta.w = 0.001, tail.prob = 0.01, 
                                method=c("derivative", "average"),
                                VaR.method=c("HS", "CornishFisher")) {
## Compute marginal ES given bootstrap data and portfolio weights.
## Marginal ES is computed either as the numerical derivative of ES wrt portfolio weight or
## as the expected fund return given portfolio return is less than or equal to portfolio VaR 
## VaR is compute either as the sample quantile or as an estimated quantile 
## using the Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of B bootstrap returns on assets in portfolio. 
## w          n x 1 vector of portfolio weights
## delta.w    scalar, change in portfolio weight for computing numerical derivative
## tail.prob  scalar tail probability
## method     character, method for computing marginal ES. Valid choices are 
##            "derivative" for numerical computation of the derivative of portfolio
##            ES wrt fund portfolio weight; "average" for approximating E[Ri | Rp<=VaR]
## VaR.method character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package            
## output:
## mES        n x 1 matrix of marginal ES values for each fund
## References:
## 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
##    The Journal of Risk 5/2.
## 2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and 
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
##    Bank of Japan.
  require(PerformanceAnalytics)
  require(ff)
  method = method[1]
  VaR.method = VaR.method[1]
  if (!is.ff(bootData))
    bootData = as.matrix(bootData)
  w = as.matrix(w)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")

  n.w = nrow(w)
  
  ## portfolio VaR and ES with all assets
  if (is.ff(bootData)) {
      dummy<-TRUE; if(!dummy){i1=1;i2=2} #fake assign to make R CMD check happy
      r.p = ffrowapply( bootData[i1:i2, ]%*%w, X=bootData, RETURN=TRUE, RETCOL=1)
  } else {
    r.p = bootData %*% w
  }  
  if (VaR.method == "HS") {
    pVaR = quantile(r.p[], prob=tail.prob)
    idx = which(r.p[] <= pVaR)
    pES = -mean(r.p[idx]) 
  } else {
    pVaR = -VaR.CornishFisher(r.p[], p=(1-tail.prob))
    idx = which(r.p[] <= pVaR)
    pES = -mean(r.p[idx]) 
  }
  ##
  ## compute marginal ES
  ##
  if (method=="derivative") {
  ## compute marginal ES as derivative wrt portfolio weight  
    temp.w = w
    mES = matrix(0, n.w, 1)
    for (i in 1:n.w) {
    ## increment weight for asset i by delta.w
      temp.w[i,1] = w[i,1] + delta.w
      if (is.ff(bootData)) {
          dummy<-TRUE; if(!dummy){i1=1;i2=2} #fake assign to make R CMD check happy
          temp.r.p = ffrowapply( bootData[i1:i2, ]%*%temp.w, X=bootData, RETURN=TRUE, RETCOL=1)
      } else {
        temp.r.p = bootData %*% temp.w
      }  
      if (VaR.method == "HS") {
        pVaR.new = quantile(temp.r.p[], prob=tail.prob)
        idx = which(temp.r.p[] <= pVaR.new)
        pES.new = -mean(temp.r.p[idx])
      } else {
        pVaR.new = -VaR.CornishFisher(temp.r.p[], p=(1-tail.prob))
        idx = which(temp.r.p[] <= pVaR.new)
        pES.new = -mean(temp.r.p[idx])
      }  
      mES[i,1] = (pES.new - pES)/delta.w
    ## reset weight
      temp.w = w
    }
  } else {
  ## compute marginal ES as expected value of fund return given portfolio 
  ## return is less than or equal to portfolio VaR
    mES = -as.matrix(colMeans(bootData[idx,,drop=FALSE]))  
  }
## compute correction factor so that sum of weighted marginal ES adds to portfolio VaR  
cf = as.numeric( pES / sum(mES*w) )
return(cf*mES)
}

bootstrapComponentES <- function(bootData, w, delta.w = 0.001, tail.prob = 0.01, 
                                  method=c("derivative", "average"),
                                  VaR.method=c("HS", "CornishFisher")) {
## Compute component ES given bootstrap data and portfolio weights.
## Marginal ES is computed either as the derivative of ES wrt portfolio weight or
## as the expected fund return given portfolio return is less than or equal to 
## portfolio VaR. VaR is compute either as the sample quantile or as an estimated 
## quantile using the Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of bootstrap returns on assets in portfolio. 
## w          n x 1 vector of portfolio weights
## delta.w    scalar, change in portfolio weight for computing numerical derivative
## tail.prob  scalar tail probability
## method     character, method for computing marginal ES. Valid choices are 
##            "derivative" for numerical computation of the derivative of portfolio
##            ES wrt fund portfolio weight; "average" for approximating E[Ri | Rp<=VaR]
## VaR.method character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package            
## output:
## cES      n x 1 matrix of component VaR values for each fund
## References:
## 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
##    The Journal of Risk 5/2.
## 2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and 
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
##    Bank of Japan.
  require(PerformanceAnalytics)
  require(ff)
  method = method[1]
  VaR.method = VaR.method[1]
  if (!is.ff(bootData))
    bootData = as.matrix(bootData)
  w = as.matrix(w)
   
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  
  mES = bootstrapMarginalES(bootData, w, delta.w, tail.prob, 
                            method, VaR.method)
  cES = mES * w
  return(cES)
}

bootstrapESreport <- function(bootData, w, delta.w = 0.001, tail.prob = 0.01, 
                              method=c("derivative", "average"),
                              VaR.method=c("HS", "CornishFisher"),
                              nav, nav.p, fundName, fundStrategy) {
## compute ES report for collection of assets in a portfolio given
## simulated (bootstrapped) return data 
## Report format follows that of Excel VaR report
## inputs:
## bootData       B x n matrix of B bootstrap returns on assets in portfolio. 
## w              n x 1 vector of portfolio weights
## delta.w        scalar, change in portfolio weight for computing numerical derivative.
##                Default value is 0.01.
## tail.prob      scalar tail probability
## method         character, method for computing marginal ES. Valid choices are 
##                "derivative" for numerical computation of the derivative of portfolio
##                ES wrt fund portfolio weight; "average" for approximating E[Ri | Rp<=VaR]
## VaR.method     character, method for computing VaR. Valid choices are "HS" for
##                historical simulation (empirical quantile); "CornishFisher" for
##                modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##                computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##                package           
## nav            n x 1 vector of net asset values in each fund
## nav.p          scalar, net asset value of portfolio
## fundName       n x 1 vector of fund names
## fundStrategy   n x 1 vector of fund strategies
## output:
## ESreport.df   dataframe with the following columns
## w             n x 1 vector of asset weights
## aES           n x 1 vector of asset specific ES values
## mES           n x 1 vector of asset specific marginal ES values
## iES           n x 1 vector of asset specific incremental ES values
## cES           n x 1 vector of asset specific component ES values
## pcES          n x 1 vector of asset specific percent contribution to ES values
##
##  To-do: Add information for cash position. 
  require(PerformanceAnalytics)
  require(ff)
  method = method[1]
  VaR.method = VaR.method[1]
  if (!is.ff(bootData))
    bootData = as.matrix(bootData)
  w = as.matrix(w)
  nav = as.matrix(nav)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
 
  asset.ES = bootstrapES(bootData, tail.prob, VaR.method)
  portfolio.ES = bootstrapPortfolioES(bootData, w, tail.prob, VaR.method)
  marginal.ES = bootstrapMarginalES(bootData, w, delta.w, tail.prob, 
                                      method, VaR.method)
  component.ES = marginal.ES*w
  incremental.ES = bootstrapIncrementalES(bootData, w, tail.prob, VaR.method)
  
  if (is.ff(bootData)) {
    dummy<-TRUE; if(!dummy){i1=1;i2=2} #fake assign to make R CMD check happy
      
    mean.vals = ffrowapply(colMeans(bootData[i1:i2,,drop=FALSE]), 
                           X=bootData, RETURN=TRUE, CFUN="cmean")
    sd.vals = ffrowapply(colMeans(bootData[i1:i2,,drop=FALSE]^2) - colMeans(bootData[i1:i2,,drop=FALSE])^2,
                         X=bootData, RETURN=TRUE, CFUN="cmean")
    sd.vals = sqrt(sd.vals)                                            
  } else {
    mean.vals = colMeans(bootData)
    sd.vals = apply(bootData, 2, sd)
  }
  
  ESreport.df = data.frame(Strategy = fundStrategy,
                            Net.Asset.Value = nav,
                            Allocation = w,
                            Mean = mean.vals,
                            Std.Dev = sd.vals,
                            Asset.ES = asset.ES,
                            cES = component.ES,
                            cES.dollar = component.ES*nav.p, 
                            pcES = component.ES/portfolio.ES,                                                 
                            iES = incremental.ES,
                            iES.dollar = incremental.ES*nav.p,
                            mES = marginal.ES, 
                            mES.dollar = marginal.ES*nav.p)
  rownames(ESreport.df) = fundName                            
  return(ESreport.df)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# This file originally written and contributed by Eric Zivot, 2010
# package Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
