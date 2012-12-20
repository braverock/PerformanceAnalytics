#' calculate Treynor Ratio or modified Treynor Ratio of excess return over CAPM beta
#' 
#' The Treynor ratio is similar to the Sharpe Ratio, except it uses beta as the
#' volatility measure (to divide the investment's excess return over the beta).
#' 
#' To calculate modified Treynor ratio, we divide the numerator by the systematic risk
#' instead of the beta.
#'
#' Equation:
#' \deqn{TreynorRatio = \frac{\overline{(R_{a}-R_{f})}}{\beta_{a,b}}}{(mean(Ra-Rf))/(Beta(Ra,Rb))}
#' \deqn{ModifiedTreynorRatio = \frac{r_p - r_f}{\sigma_s}}{ModifiedTreynorRatio = (Rp - Rf)/sytematic risk}
#' 
#' @aliases TreynorRatio
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param modified a boolean to decide whether to return the Treynor ratio or
#' Modified Treynor ratio
#' @author Peter Carl, Matthieu Lestel
#' @seealso \code{\link{SharpeRatio}} \code{\link{SortinoRatio}}
#' \code{\link{CAPM.beta}}
#' @references \url{http://en.wikipedia.org/wiki/Treynor_ratio}, 
#' Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.77
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon) 
#' data(managers)
#' round(TreynorRatio(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12),4) 
#' round(TreynorRatio(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf = managers[,10,drop=FALSE]),4) 
#' round(TreynorRatio(managers[,1:6], managers[,8,drop=FALSE], Rf=.035/12),4) 
#' round(TreynorRatio(managers[,1:6], managers[,8,drop=FALSE], Rf = managers[,10,drop=FALSE]),4)
#' round(TreynorRatio(managers[,1:6], managers[,8:7,drop=FALSE], Rf=.035/12),4) 
#' round(TreynorRatio(managers[,1:6], managers[,8:7,drop=FALSE], Rf = managers[,10,drop=FALSE]),4)
#'
#' print(TreynorRatio(portfolio_bacon[,1], portfolio_bacon[,2], modified = TRUE)) #expected 0.7975 
#'
#' print(TreynorRatio(managers['1996',1], managers['1996',8], modified = TRUE))
#' print(TreynorRatio(managers['1996',1:5], managers['1996',8], modified = TRUE)) 
#' 
#' @export

TreynorRatio <-
function (Ra, Rb, Rf = 0, scale = NA, modified = FALSE)
{ # @author Peter Carl, Matthieu Lestel
  
  
  # DESCRIPTION:
  #
  
  # FUNCTION:
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  if(!is.null(dim(Rf)))
    Rf = checkData(Rf)

  if(is.na(scale)) {
    freq = periodicity(Ra)
    switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
    )
  }
  
  Ra.ncols = NCOL(Ra) 
  Rb.ncols = NCOL(Rb)
  
  pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
  
  xRa = Return.excess(Ra, Rf)
  xRb = Return.excess(Rb, Rf)
    
  tr <-function (xRa, xRb, scale)
  {
    beta = CAPM.beta(xRa, xRb)
    TR = Return.annualized(xRa, scale = scale)/beta
    TR
  }
  
  mtr <-function (xRa, xRb, scale)
  {
    mTR = Return.annualized(xRa, scale = scale)/SystematicRisk(xRa, xRb, Rf=0)
    mTR
  }
  
  if(modified)
    result = apply(pairs, 1, FUN = function(n, xRa, xRb, scale) mtr(xRa[,n[1]], xRb[,n[2]], scale), xRa = xRa, xRb = xRb, scale = scale)
  else
    result = apply(pairs, 1, FUN = function(n, xRa, xRb, scale) tr(xRa[,n[1]], xRb[,n[2]], scale), xRa = xRa, xRb = xRb, scale = scale)
  
  if(length(result) ==1)
    return(result)
  else {
    dim(result) = c(Ra.ncols, Rb.ncols)
    colnames(result) = paste("Treynor Ratio:", colnames(Rb))
    rownames(result) = colnames(Ra)
    return(t(result))
  }
}




###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
