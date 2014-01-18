## valueAtRiskFunctions.r          
##
## author: Eric Zivot
## created: December 6, 2008
## updated: October 1, 2009
##
## purpose: functions for computing value-at-risk
##
## Functions:
## normalVaR
## normalPortfolioVaR
## normalMarginalVaR
## normalComponentVaR
## normalIncrementalVaR
## normalVaRreport
## modifiedVaR
## modifiedPortfolioVaR
## modifiedMarginalVaR          needs to be finished anc checked
## modifiedComponentVaR         needs to be finished anc checked
## modifiedIncrementalVaR
## modifiedVaRreport            needs to be finished anc checked
## bootstrapVaR
## bootstrapPortfolioVaR
## bootstrapIncrementalVaR
## bootstrapMarginalVaR
## bootstrapComponentVaR
## bootstrapVaRreport
##
## Comments:
## 1. Cornish-Fisher (modified) VaR functions are in the package PerformanceAnalytics
## 2. Standard errors for VaR calculations can be computed using the bootstrap
##
## References:
## 1. Dowd, K. (2002). Measuring Market Risk, John Wiley and Sons
## 2. Jorian, P. (2007). Value at Risk, Third Edition, McGraw Hill
## 3. Hallerback (2003). "Decomposing Portfolio Value-at-Risk: A General Analysis",
##    The Journal of Risk 5/2.
## 4. Boudt, C., B. Peterson and C. Croux (2008). "Estimation and Decomposition of Downside
##    Risk for Portfolios with Non-Normal Returns," Journal of Risk.
## 5. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and 
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization",
##    Bank of Japan.
## 6. Gourieroux, C., P. Laurent and O. Scaliet (2000). "Sensitivity Analysis of Values at
##    Risk", Journal of Empirical Finance.

##
## Normal VaR functions for portfolio VaR report
##

normalVaR <- function(mu, sigma, tail.prob = 0.01) {
## compute normal VaR for collection of assets given mean and sd vector
## inputs:
## mu       n x 1 vector of expected returns
## sigma    n x 1 vector of standard deviations
## tail.prob  scalar tail probability
## output:
## VaR      n x 1 vector of left tail return quantiles returned as a positive number
## References:
## Jorian (2007) pg 111.
  mu = as.matrix(mu)
  sigma = as.matrix(sigma)
  if ( nrow(mu) != nrow(sigma) )
    stop("mu and sigma must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  VaR = mu + sigma*qnorm(tail.prob)
  return(-VaR)
}

normalPortfolioVaR <- function(mu, Sigma, w, tail.prob = 0.01) {
## compute normal portfolio VaR given portfolio weights, mean vector and
## covariance matrix
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## pVaR      scalar left tail return quantile of normal portfolio distn returned 
##           as a positive number    
## References:
## Jorian (2007) pg 162
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  pVaR = crossprod(w, mu) + sqrt( t(w) %*% Sigma %*% w ) * qnorm(tail.prob)
  return(as.numeric(-pVaR))
}

normalMarginalVaR <- function(mu, Sigma, w, tail.prob = 0.01) {
## compute normal marginal VaR for collection of assets in a portfolio given
## mean vector and covariance matrix.
## marginal VaR is defined as the derivative of VaR wrt portfolio weight
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## mVaR      n x 1 vector of left tail return quantiles returned as a positive number
## References:
## Jorian (2007) pg. 167
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  sigma.p = as.numeric(sqrt( t(w) %*% Sigma %*% w ))
  mVaR = mu + Sigma %*% w * qnorm(tail.prob) / sigma.p
  return(-mVaR)
}

normalComponentVaR <- function(mu, Sigma, w, tail.prob = 0.01) {
## compute normal component VaR for collection of assets in a portfolio given
## mean vector and covariance matrix
## component VaR is defined as the asset specific contribution to portfolio VaR
## such that portfolio VaR is equal to the sum of the asset specific contributions.
## By Euler's theorem component VaR = marginal VaR * portfolio weight
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## cVaR      n x 1 vector of component VaR quantities
## References:
## Jorian (2007) pg. 173
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  mVaR = normalMarginalVaR(mu, Sigma, w, tail.prob)
  cVaR = mVaR * w
  return(cVaR)
}

normalIncrementalVaR <- function(mu, Sigma, w, tail.prob = 0.01) {
## compute normal incremental VaR given portfolio weights, mean vector and
## covariance matrix
## Incremental VaR is defined as the change in portfolio VaR that occurs
## when an asset is removed from the portfolio
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## iVaR      n x 1 vector of incremental VaR values
## References:
## Jorian (2007) pg. 168
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
  ## portfolio VaR with all assets
  pVaR = crossprod(w, mu) + sqrt( t(w) %*% Sigma %*% w ) * qnorm(tail.prob)
  temp.w = w
  iVaR = matrix(0, n.w, 1)
  for (i in 1:n.w) {
  ## set weight for asset i to zero and renormalize
    temp.w[i,1] = 0
    temp.w = temp.w/sum(temp.w)
    pVaR.new = crossprod(temp.w, mu) + sqrt( t(temp.w) %*% Sigma %*% temp.w ) * qnorm(tail.prob)    
    iVaR[i,1] = pVaR.new - pVaR
  ## reset weight
    temp.w = w
  }
  return(-iVaR)
}

normalVaRreport <- function(mu, Sigma, w, nav, nav.p, fundName, fundStrategy, 
                            tail.prob = 0.01) {
## compute normal VaR report for collection of assets in a portfolio given
## mean vector and covariance matrix. Report format follows that of Excel VaR report
## inputs:
## mu             n x 1 vector of expected returns
## Sigma          n x n return covariance matrix
## w              n x 1 vector of portfolio weights
## nav            n x 1 vector of net asset values in each fund
## vav.p          scalar, net asset value of portfolio
## fundName       n x 1 vector of fund names
## fundStrategy   n x 1 vector of fund strategies
## tail.prob      scalar tail probability
## output:
## VaRreport.df   dataframe with n rows and the following columns
##  Strategy         character strategy group abbreviation
##  Net.Asset.Value  net asset value
##  Allocation       asset weights
##  Mean             mean return
##  Std.Dev          standard deviation of return
##  Asset.Var        asset specific VaR values
##  cVaR             asset specific component VaR values
##  cVaR.dollar      asset specific component VaR values multiplied by net asset value
##  pcVaR            asset specific percent contribution to VaR values
##  iVaR             asset specific incremental VaR values
##  iVaR.dollar      asset specific incremental VaR values multiplied by net asset value
##  mVaR             asset specific marginal VaR values
##  mVaR.dollar      asset specific marginal VaR values multiplied by net asset value

##
##  To-do: Add information for cash position. 

  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  nav = as.matrix(nav)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  
  asset.VaR = normalVaR(mu, sqrt(diag(Sigma)), tail.prob)
  portfolio.VaR = normalPortfolioVaR(mu, Sigma, w, tail.prob)
  marginal.VaR = normalMarginalVaR(mu, Sigma, w, tail.prob)
  component.VaR = normalComponentVaR(mu, Sigma, w, tail.prob)
  incremental.VaR = normalIncrementalVaR(mu, Sigma, w, tail.prob)
  VaRreport.df = data.frame(Strategy = fundStrategy,
                            Net.Asset.Value = nav,
                            Allocation = w,
                            Mean = mu,
                            Std.Dev = sqrt(diag(Sigma)),
                            Asset.VaR = asset.VaR,
                            cVaR = component.VaR,
                            cVaR.dollar = component.VaR*nav.p, 
                            pcVaR = component.VaR/portfolio.VaR,                                                 
                            iVaR = incremental.VaR,
                            iVaR.dollar = incremental.VaR*nav.p,
                            mVaR = marginal.VaR, 
                            mVaR.dollar = marginal.VaR*nav.p)
  rownames(VaRreport.df) = fundName                            
  return(VaRreport.df)
}

##
## modifed (Cornish-Fisher) VaR functions. These functions make use 
## of several functions in the PerformanceAnalytics package
##

modifiedVaR <- function(mu, sigma, skew, kurt, tail.prob = 0.01) {
## compute modified (Cornish-Fisher) VaR for collection of assets given mean and 
## sd vectors. 
## inputs:
## mu       n x 1 vector of expected returns
## sigma    n x 1 vector of standard deviations
## skew     n x 1 vector of skewness values
## kurt     n x 1 vector of kurtosis values
## tail.prob  scalar tail probability
## output:
## mVaR      n x 1 vector of left tail return quantiles returned as a positive number
## References:
## Jorian (2007) Value At Risk, pg. 273
## Boudt, Peterson and Croux (2008), "Estimation and Decomposition of Downside
## Risk for Portfolios with Non-Normal Returns," Journal of Risk.
  mu = as.matrix(mu)
  sigma = as.matrix(sigma)
  if ( nrow(mu) != nrow(sigma) )
    stop("mu and sigma must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  zc = qnorm(tail.prob)
  Zcf = zc + (((zc^2 - 1) * skew)/6) + (((zc^3 - 3 * zc) * 
                kurt)/24) - ((((2 * zc^3) - 5 * zc) * skew^2)/36)
  mVaR = mu + sigma*Zcf
  return(-mVaR)
}

modifiedPortfolioVaR <- function(mu, Sigma, M3, M4, w, tail.prob = 0.01) {
## compute modified (Cornish-Fisher) portfolio VaR given portfolio weights, mean vector,
## covariance matrix, co-skewness matrix and co-kurtosis matrix
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## M3         n x n^2 co-skewness matrix
## M4         n x m^3 co-kurtosis matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## pMVaR     scalar left tail return quantile of portfolio distn returned 
##           as a positive number
## References:
## 1. Calculations follow Boudt et al (2008) "Estimation and Decomposition of Downside
##    Risk for Portfolios with Non-Normal Returns," Journal of Risk, and make use
##    of the function mVaR.MM in the PerformanceAnalytics package
  require(PerformanceAnalytics) 
  p = 1 - tail.prob
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")  
  pMVaR = mVaR.MM(w, mu, Sigma, M3, M4, p)
  return(pMVaR)
}

modifiedMarginalVaR <- function(mu, Sigma, M3, M4, w, tail.prob = 0.01) {
## compute Cornish-Fisher (modified) marginal VaR for collection of assets in a 
## portfolio given mean vector, covariance matrix, co-skewness matrix and co-kurtosis
## matrix. 
## marginal VaR is defined as the derivative of VaR wrt portfolio weight
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## mVaR      n x 1 vector of left tail return quantiles returned as a positive number
## References: 
## Boudt, Peterson and Croux (2008), "Estimation and Decomposition of Downside
## Risk for Portfolios with Non-Normal Returns," Journal of Risk.
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  sigma.p = as.numeric(sqrt( t(w) %*% Sigma %*% w ))
  mVaR = mu + Sigma %*% w * qnorm(tail.prob) / sigma.p
  return(-mVaR)
}

modifiedComponentVaR <- function(mu, Sigma, M3, M4, w, tail.prob = 0.01) {
## compute Cornish-Fisher (modified) component VaR for collection of assets in 
## a portfolio given mean vector, covariance matrix, co-skewness matrix and co-kurtosis
## matrix.
## component VaR is defined as the asset specific contribution to portfolio VaR
## such that portfolio VaR is equal to the sum of the asset specific contributions.
## By Euler's theorem component VaR = marginal VaR * portfolio weight
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## cVaR      n x 1 vector of component VaR quantities
## References: 
## Boudt, Peterson and Croux (2008), "Estimation and Decomposition of Downside
## Risk for Portfolios with Non-Normal Returns," Journal of Risk.
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  mVaR = modifiedMarginalVaR(mu, Sigma, M3, M4, w, tail.prob)
  cVaR = mVaR * w
  return(cVaR)
}

modifiedIncrementalVaR <- function(mu, Sigma, M3, M4, w, tail.prob = 0.01) {
## compute modified incremental VaR given portfolio weights, mean vector,
## covariance matrix, co-skewness matrix and co-kurtosis matrix
## Incremental VaR is defined as the change in portfolio VaR that occurs
## when an asset is removed from the portfolio
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## iVaR      n x 1 vector of incremental VaR values
## References:
## Jorian (2007) pg. 168
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
  ## portfolio VaR with all assets
  pVaR = modifiedPortfolioVaR(mu, Sigma, M3, M4, w, tail.prob)
  temp.w = w
  iVaR = matrix(0, n.w, 1)
  for (i in 1:n.w) {
  ## set weight for asset i to zero and renormalize
    temp.w[i,1] = 0
    temp.w = temp.w/sum(temp.w)
    pVaR.new = modifiedPortfolioVaR(mu, Sigma, M3, M4, w, tail.prob)    
    iVaR[i,1] = pVaR.new - pVaR
  ## reset weight
    temp.w = w
  }
  return(iVaR)
}

modifiedVaRreport <- function(mu, Sigma, skew, kurt, M3, M4, w, nav, nav.p, fundName, fundStrategy, 
                              tail.prob = 0.01) {
## compute Cornish-Fisher (modified) VaR report for collection of assets in a portfolio given
## mean vector, covariance matrix, co-skewness matrix and co-kurtosis matrix. 
## Report format follows that of Excel VaR report
## inputs:
## mu             n x 1 vector of expected returns
## Sigma          n x n return covariance matrix
## skew           n x 1 vector of skewness values
## kurt           n x 1 vector of kurtosis values
## M3             n x n^2 co-skewness matrix
## M4             n x m^3 co-kurtosis matrix
## w              n x 1 vector of portfolio weights
## nav            n x 1 vector of net asset values in each fund
## vav.p          scalar, net asset value of portfolio
## fundName       n x 1 vector of fund names
## fundStrategy   n x 1 vector of fund strategies
## tail.prob      scalar tail probability
## output:
## VaRreport.df   dataframe with n rows and the following columns
##  Strategy         character strategy group abbreviation
##  Net.Asset.Value  net asset value
##  Allocation       asset weights
##  Mean             mean return
##  Std.Dev          standard deviation of return
##  Asset.Var        asset specific VaR values
##  cVaR             asset specific component VaR values
##  cVaR.dollar      asset specific component VaR values multiplied by net asset value
##  pcVaR            asset specific percent contribution to VaR values
##  iVaR             asset specific incremental VaR values
##  iVaR.dollar      asset specific incremental VaR values multiplied by net asset value
##  mVaR             asset specific marginal VaR values
##  mVaR.dollar      asset specific marginal VaR values multiplied by net asset value
##
##  To-do: Add information for cash position. 


## extract asset skewness and excess kurtosis values from co-skewness and co-kurtosis
## matrices

## skew = M3[]
## kurt = M4[]

## where each VaR component has been multiplied by -1
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  nav = as.matrix(nav)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  
  asset.VaR = modifiedVaR(mu, sqrt(diag(Sigma)), skew, kurt, tail.prob)
  portfolio.VaR = modifiedPortfolioVaR(mu, Sigma, M3, M4, w, tail.prob)
  marginal.VaR = modifiedMarginalVaR(mu, Sigma, M3, M4, w, tail.prob)
  component.VaR = marginal.VaR*w
  incremental.VaR = modifiedIncrementalVaR(mu, Sigma, M3, M4, w, tail.prob)
  VaRreport.df = data.frame(Strategy = fundStrategy,
                            Net.Asset.Value = nav,
                            Allocation = w,
                            Mean = mu,
                            Std.Dev = sqrt(diag(Sigma)),
                            Asset.VaR = asset.VaR,
                            cVaR = component.VaR,
                            cVaR.dollar = component.VaR*nav.p, 
                            pcVaR = component.VaR/portfolio.VaR,                                                 
                            iVaR = incremental.VaR,
                            iVaR.dollar = incremental.VaR*nav.p,
                            mVaR = marginal.VaR, 
                            mVaR.dollar = marginal.VaR*nav.p)
  rownames(VaRreport.df) = fundName                            
  return(VaRreport.df)
}

##
## To-do: Add functions for plotting risk budgeting information
##

##
## VaR functions for use with simulated (bootstrapped) return data
##

bootstrapVaR <- function(bootData, tail.prob = 0.01, 
                         method=c("HS", "CornishFisher")) {
## Compute VaR given bootstrap data VaR is computed either as the sample quantile or as
## an estimated quantile using the Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of bootstrap returns on n assets in portfolio
## tail.prob  scalar tail probability
## method     character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package
## output:
## VaR      n x 1 matrix of VaR values for each asset
## References:

  require(PerformanceAnalytics)
  method = method[1]
  bootData = as.matrix(bootData)
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  if (method == "HS") {
  ## use empirical quantile
    VaR = -as.matrix(apply(bootData, 2, FUN=quantile, prob=tail.prob))
  } else {
  ## use Cornish-Fisher expansion
    VaR = t(as.matrix(VaR.CornishFisher(bootData, p=(1-tail.prob))))
  }
return(VaR)
}

bootstrapPortfolioVaR <- function(bootData, w, tail.prob = 0.01, 
                                  method=c("HS", "CornishFisher")) {
## Compute portfolio VaR given bootstrap data and portfolio weights. VaR is 
## computed either as the sample quantile or as an estimated quantile using the 
## Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of bootstrap returns on n assets in portfolio
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## method     character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package
## output:
## pVaR       scalar, portfolio VaR
## References:

  require(PerformanceAnalytics)
  method = method[1]
  bootData = as.matrix(bootData)
  w = as.matrix(w)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  r.p = bootData %*% w  
  if (method == "HS") {
  ## use empirical quantile
    pVaR = -quantile(r.p, prob=tail.prob)
  } else {
  ## use Cornish-Fisher expansion
    pVaR = VaR.CornishFisher(r.p, p=(1-tail.prob))
  }
return(pVaR)
}


bootstrapIncrementalVaR <- function(bootData, w, tail.prob = 0.01, 
                                    method=c("HS", "CornishFisher")) {
## Compute incremental VaR given bootstrap data and portfolio weights.
## Incremental VaR is defined as the change in portfolio VaR that occurs
## when an asset is removed from the portfolio and allocation is spread equally
## among remaining assets. VaR is computed either as the sample quantile or as
## an estimated quantile using the Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of bootstrap returns on n assets in portfolio
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## method     character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package
## output:
## iVaR      n x 1 matrix of incremental VaR values for each asset
## References:
## Jorian (2007) pg. 168
## 
  require(PerformanceAnalytics)
  method = method[1]
  bootData = as.matrix(bootData)
  w = as.matrix(w)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  
  n.w = nrow(w)
  ## portfolio VaR with all assets
  r.p = bootData %*% w
  if (method == "HS") {
  ## use empirical quantile
    pVaR = -quantile(r.p, prob=tail.prob)
    temp.w = w
    iVaR = matrix(0, n.w, 1)
    for (i in 1:n.w) {
    ## set weight for asset i to zero and renormalize
      temp.w[i,1] = 0
      temp.w = temp.w/sum(temp.w)
      temp.r.p = bootData %*% temp.w
      pVaR.new = -quantile(temp.r.p, prob=tail.prob)    
      iVaR[i,1] = pVaR.new - pVaR
    ## reset weight
      temp.w = w
  }
  } else {
  ## use Cornish-Fisher VaR
    pVaR = VaR.CornishFisher(r.p, p=(1-tail.prob))
    temp.w = w
    iVaR = matrix(0, n.w, 1)
    for (i in 1:n.w) {
    ## set weight for asset i to zero and renormalize
      temp.w[i,1] = 0
      temp.w = temp.w/sum(temp.w)
      temp.r.p = bootData %*% temp.w
      pVaR.new = VaR.CornishFisher(temp.r.p, p=(1-tail.prob))   
      iVaR[i,1] = pVaR.new - pVaR
    ## reset weight
      temp.w = w
  }
  }
  return(iVaR)
}

bootstrapMarginalVaR <- function(bootData, w, delta.w = 0.001, h=NULL, tail.prob = 0.01, 
                                 method=c("derivative", "average"),
                                 VaR.method=c("HS", "CornishFisher")) {
## Compute marginal VaR given bootstrap data and portfolio weights.
## Marginal VaR is computed either as the numerical derivative of VaR wrt portfolio weight or
## as the expected fund return given portfolio return is equal to portfolio VaR 
## VaR is compute either as the sample quantile or as an estimated quantile 
## using the Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of bootstrap returns on assets in portfolio. 
## w          n x 1 vector of portfolio weights
## delta.w    scalar, change in portfolio weight for computing numerical derivative
## h          integer, number of obvs on each side of VaR. Default is h=round(sqrt(B)/2)
## tail.prob  scalar tail probability
## method     character, method for computing marginal VaR. Valid choices are 
##            "derivative" for numerical computation of the derivative of portfolio
##            VaR wrt fund portfolio weight; "average" for approximating E[Ri | Rp=VaR]
## VaR.method character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package            
## output:
## mVaR      n x 1 matrix of marginal VaR values for each fund
## References:
## 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
##    The Journal of Risk 5/2.
## 2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and 
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
##    Bank of Japan.
  require(PerformanceAnalytics)
  method = method[1]
  VaR.method = VaR.method[1]
  bootData = as.matrix(bootData)
  w = as.matrix(w)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")

  # determine number of obvs to average around VaR
  if (is.null(h)) {
    h = round(sqrt(nrow(bootData)))
  } else h = round(h)
  
  n.w = nrow(w)
  ## portfolio VaR with all assets
  r.p = bootData %*% w
  if (VaR.method == "HS") {
    pVaR = -quantile(r.p, prob=tail.prob)
  } else {
    pVaR = VaR.CornishFisher(r.p, p=(1-tail.prob))
  }
  ##
  ## compute marginal VaR
  ##
  if (method=="derivative") {
  ## compute marginal var as derivative wrt portfolio weight  
    temp.w = w
    mVaR = matrix(0, n.w, 1)
    for (i in 1:n.w) {
    ## increment weight for asset i by delta.w
      temp.w[i,1] = w[i,1] + delta.w
      temp.r.p = bootData %*% temp.w
      if (VaR.method == "HS") {
        pVaR.new = -quantile(temp.r.p, prob=tail.prob)
      } else {
        pVaR.new = VaR.CornishFisher(temp.r.p, p=(1-tail.prob))
      }  
      mVaR[i,1] = (pVaR.new - pVaR)/delta.w
    ## reset weight
      temp.w = w
    }
  } else {
  ## compute marginal VaR as expected value of fund return given portfolio 
  ## return is equal to portfolio VaR
    r.p.sort = sort(r.p)
    idx.lower = which(r.p.sort <= -pVaR)
    idx.upper = which(r.p.sort > -pVaR)
    r.p.vals = c(r.p.sort[tail(idx.lower,n=h)], r.p.sort[head(idx.upper,n=h)])
    idx = which(r.p %in% r.p.vals)
    mVaR = -as.matrix(colMeans(bootData[idx,]))  
  }
## compute correction factor so that sum of weighted marginal VaR adds to portfolio VaR  
cf = as.numeric( pVaR / sum(mVaR*w) )
return(cf*mVaR)
}

bootstrapComponentVaR <- function(bootData, w, delta.w = 0.001, h=NULL, tail.prob = 0.01, 
                                  method=c("derivative", "average"),
                                  VaR.method=c("HS", "CornishFisher")) {
## Compute component VaR given bootstrap data and portfolio weights.
## Marginal VaR is computed either as the derivative of VaR wrt portfolio weight or
## as the expected fund return given portfolio return is equal to portfolio VaR 
## VaR is compute either as the sample quantile or as an estimated quantile 
## using the Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of bootstrap returns on assets in portfolio. 
## w          n x 1 vector of portfolio weights
## delta.w    scalar, change in portfolio weight for computing numerical derivative
## h          integer, number of obvs on each side of VaR. Default is h=round(sqrt(B)/2)
## tail.prob  scalar tail probability
## method     character, method for computing marginal VaR. Valid choices are 
##            "derivative" for numerical computation of the derivative of portfolio
##            VaR wrt fund portfolio weight; "average" for approximating E[Ri | Rp=VaR]
## VaR.method character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package            
## output:
## cVaR      n x 1 matrix of component VaR values for each fund
## References:
## 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
##    The Journal of Risk 5/2.
## 2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and 
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
##    Bank of Japan.
  require(PerformanceAnalytics)
  method = method[1]
  VaR.method = VaR.method[1]
  bootData = as.matrix(bootData)
  w = as.matrix(w)
  
  # determine number of obvs to average around VaR
  if (is.null(h)) {
    h = round(sqrt(nrow(bootData)))
  } else h = round(h)
  
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  
  mVaR = bootstrapMarginalVaR(bootData, w, delta.w, h, tail.prob, 
                              method, VaR.method)
  cVaR = mVaR * w
  return(cVaR)
}

bootstrapVaRreport <- function(bootData, w, delta.w = 0.001, h=NULL, tail.prob = 0.01, 
                               method=c("derivative", "average"),
                               VaR.method=c("HS", "CornishFisher"),
                               nav, nav.p, fundName, fundStrategy) {
## compute VaR report for collection of assets in a portfolio given
## simulated (bootstrapped) return data 
## Report format follows that of Excel VaR report
## inputs:
## bootData       B x n matrix of bootstrap returns on assets in portfolio. 
## w              n x 1 vector of portfolio weights
## delta.w        scalar, change in portfolio weight for computing numerical derivative
## h              integer, number of obvs on each side of VaR. Default is h=round(sqrt(B)/2)
## tail.prob      scalar tail probability
## method         character, method for computing marginal VaR. Valid choices are 
##                "derivative" for numerical computation of the derivative of portfolio
##                VaR wrt fund portfolio weight; "average" for approximating E[Ri | Rp=VaR]
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
## VaRreport.df   dataframe with n rows and the following columns
##  Strategy         character strategy group abbreviation
##  Net.Asset.Value  net asset value
##  Allocation       asset weights
##  Mean             mean return
##  Std.Dev          standard deviation of return
##  Asset.Var        asset specific VaR values
##  cVaR             asset specific component VaR values
##  cVaR.dollar      asset specific component VaR values multiplied by net asset value
##  pcVaR            asset specific percent contribution to VaR values
##  iVaR             asset specific incremental VaR values
##  iVaR.dollar      asset specific incremental VaR values multiplied by net asset value
##  mVaR             asset specific marginal VaR values
##  mVaR.dollar      asset specific marginal VaR values multiplied by net asset value
##
##  To-do: Add information for cash position. 
  require(PerformanceAnalytics)
  method = method[1]
  VaR.method = VaR.method[1]
  bootData = as.matrix(bootData)
  w = as.matrix(w)
  nav = as.matrix(nav)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")

  # determine number of obvs to average around VaR
  if (is.null(h)) {
    h = round(sqrt(nrow(bootData)))
  } else h = round(h)
  
  asset.VaR = bootstrapVaR(bootData, tail.prob, VaR.method)
  portfolio.VaR = bootstrapPortfolioVaR(bootData, w, tail.prob, VaR.method)
  marginal.VaR = bootstrapMarginalVaR(bootData, w, delta.w, h, tail.prob = 0.01, 
                                      method, VaR.method)
  component.VaR = marginal.VaR*w
  incremental.VaR = bootstrapIncrementalVaR(bootData, w, tail.prob, method)
  VaRreport.df = data.frame(Strategy = fundStrategy,
                            Net.Asset.Value = nav,
                            Allocation = w,
                            Mean = colMeans(bootData),
                            Std.Dev = apply(bootData, 2, sd),
                            Asset.VaR = asset.VaR,
                            cVaR = component.VaR,
                            cVaR.dollar = component.VaR*nav.p, 
                            pcVaR = component.VaR/portfolio.VaR,                                                 
                            iVaR = incremental.VaR,
                            iVaR.dollar = incremental.VaR*nav.p,
                            mVaR = marginal.VaR, 
                            mVaR.dollar = marginal.VaR*nav.p)
  rownames(VaRreport.df) = fundName                            
  return(VaRreport.df)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# This file originally written and contributed by Eric Zivot, 2010
# package Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################