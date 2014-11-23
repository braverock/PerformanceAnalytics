#' Calculates Conditional Drawdown at Risk (CDaR)
#' 
#' @description  Conditional Drawdown at Risk (CDaR) is part of a one-parameter
#' family of risk measures called Conditional Drawdown (CDD).  These measures of
#' risk are functionals of the portfolio drawdown (underwater) curve. For some
#' value of the tolerance parameter, in the case of 
#' a single sample path, drawdown functional is defineed as the mean of the
#' worst (1 - \eqn{\alpha})% drawdowns. 
#' @details 
#' The \bold{CDD} is related to Value-at-Risk (VaR) and Conditional 
#' Value-at-Risk (CVaR) measures studied by Rockafellar and Uryasev . By
#' definition, with respect to a specified  probability level \eqn{\alpha}, the
#' \bold{\eqn{\alpha}-VaR} of a portfolio is the lowest amount \eqn{\epsilon},
#' \eqn{\alpha} such that, with probability \eqn{\alpha}, the loss will not
#' exceed \eqn{\epsilon}, \eqn{\alpha} in a specified time T, whereas the
#' \bold{\eqn{\alpha}-CVaR} is the conditional expectation of losses above that
#' amount \eqn{\epsilon}. 
#' The CDD is similar to CVaR and can be viewed as a modification of the CVaR
#' to the case when the loss-function is defined as a drawdown. CDD and CVaR are
#' conceptually related percentile-based risk performance functionals.
#'
#' Like CVaR and ETL, CDaR is defined as the mean of the worst drawdowns above 
#' a quantile.  For example, the 0.90 CDaR is the average of the worst 10%
#' drawdowns over the period.
#' Convex measure, so useful in optimization
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param p confidence level for calculation, default p=.95
#' @return
#' @author Peter Carl, Shubhankit Mohan
#' @references Chekhlov, A., Uryasev, S. and Zabarankin, M. (2000). \emph{Portfolio Optimization with Drawdown Constraints.} Research Report #2000-5. Available at SSRN: \url{http://ssrn.com/abstract=223323}
#' @references Chekhlov, A., Uryasev, S. and Zabarankin, M. (2003) \emph{Drawdown Measure in Portfolio Optimization}. Paper available at SSRN: \url{http://ssrn.com/abstract=544742}
#' @export
CDaR <- function(R, p=0.95, ...){
  # Peter Carl
  # @TODO this is just the interior function; needs multi-col framework
  R = checkData(R)
  R = na.omit(R)
  nr = nrow(R)
  dd = coredata(-PerformanceAnalytics:::Drawdowns(R))
  dd = dd[order(dd),increasing = TRUE]
  # result = -(1/((1-p)*nr))*sum(dd[((p)*nr):nr])
  dar = quantile(dd, p=0.90, type=8)
  result = -1*mean(dd[dd>dar])
  result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id:  $
#
###############################################################################