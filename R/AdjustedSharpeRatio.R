#' Adjusted Sharpe ratio of the return distribution
#'
#' Adjusted Sharpe ratio was introduced by Pezier and White (2006) to adjusts
#' for skewness and kurtosis by incorporating a penalty factor for negative skewness
#' and excess kurtosis.
#'
#' \deqn{Adjusted Sharpe Ratio = SR * [1 + (\frac{S}{6}) * SR - (\frac{K - 3}{24}) * SR^2]}{Adjusted Sharpe ratio = SR x [1 + (S/6) x SR - ((K-3) / 24) x SR^2]}
#'
#' where \eqn{SR} is the sharpe ratio with data annualized, \eqn{S} is the skewness and \eqn{K} is the kurtosis
#' 
#' @aliases AdjustedSharpeRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rf the risk free rate
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel, Brian G. Peterson
#' @seealso \code{\link{SharpeRatio.annualized}}
#' @references 
#' 
#' Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.99
#' 
#' Pezier, Jaques and White, Anthony. 2006. The Relative Merits of Investable 
#' Hedge Fund Indices and of Funds of Hedge Funds in Optimal Passive Portfolios.
#' \url{http://econpapers.repec.org/paper/rdgicmadp/icma-dp2006-10.htm}
#' 
###keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon)
#' print(AdjustedSharpeRatio(portfolio_bacon[,1])) #expected 0.7591435
#'
#' data(managers)
#' print(AdjustedSharpeRatio(managers['1996']))
#' @export
AdjustedSharpeRatio <- function (R, Rf = 0, ...)
{
  if (ncol(R)==1 || is.null(R) || is.vector(R)) {
    R = na.omit(R)
    if(length(R)<2) return(NA)
    SR = SharpeRatio.annualized(R, Rf, ...)
    K = kurtosis(R, method = "moment")
    S = skewness(R)
    result = SR*(1+(S/6)*SR-((K-3)/24)*SR^2)
    return(result)
  } else {
    result = apply(R, MARGIN = 2, AdjustedSharpeRatio, Rf = Rf, ...)
    result<-t(result)
    colnames(result) = colnames(R)
    rownames(result) = paste("Adjusted Sharpe ratio (Risk free = ",Rf,")", sep="")
    return(result)
  }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2018 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
