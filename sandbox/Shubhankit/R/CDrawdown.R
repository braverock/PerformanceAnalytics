#' @title Chekhlov Conditional Drawdown at Risk
#' 
#' @description  A new one-parameter family of risk measures called Conditional Drawdown (CDD) has
#'been proposed. These measures of risk are functionals of the portfolio drawdown (underwater) curve considered in active portfolio management. For some value of the tolerance
#' parameter, in the case of a single sample path, drawdown functional is defineed as
#'the mean of the worst (1 - \eqn{\alpha})% drawdowns. 
#'@details 
#'The \bold{CDD} is related to Value-at-Risk (VaR) and Conditional Value-at-Risk
#'(CVaR) measures studied by Rockafellar and Uryasev . By definition, with
#'respect to a specified  probability level \eqn{\alpha}, the \bold{\eqn{\alpha}-VaR} of a portfolio is the lowest
#'amount \eqn{\epsilon}
#', \eqn{\alpha} such that, with probability \eqn{\alpha}, the loss will not exceed \eqn{\epsilon}
#', \eqn{\alpha} in a specified
#'time T, whereas the \bold{\eqn{\alpha}-CVaR} is the conditional expectation of losses above that
#'amount \eqn{\epsilon}
#'. Various issues about VaR methodology were discussed by Jorion .
#'The CDD is similar to CVaR and can be viewed as a modification of the CVaR
#'to the case when the loss-function is defined as a drawdown. CDD and CVaR are
#'conceptually related percentile-based risk performance functionals.
#' @param Ra return vector of the portfolio
#' @param p confidence interval
#' @author Peter Carl, Brian Peterson, Shubhankit Mohan
#' @references Chekhlov, Alexei, Uryasev, Stanislav P. and Zabarankin, Michael, \emph{Drawdown Measure in Portfolio Optimization} (June 25, 2003). Available at SSRN: \url{http://ssrn.com/abstract=544742} or \url{http://dx.doi.org/10.2139/ssrn.544742}
#' @keywords Conditional Drawdown models
#' @examples
#' 
#'     library(PerformanceAnalytics)
#' data(edhec)
#' CDrawdown(edhec)
#' @rdname Cdrawdown
#' @export 

CDrawdown <-
  function (R,p=0.90, ...)
  {
    y = checkData(R, method = "xts")
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)
    
    for(column in 1:columns) {
      x = y[,column]
      drawdown = findDrawdowns(x)
      threshold= ES(x,p)[1]
      total = length(drawdown$return)
      num = length(drawdown$return[drawdown$return>threshold])
      cva1= (((num/total)-p)/(1-p))*threshold
      cva2=sum(drawdown$return)/((1-p)*total)
      z = c((cva1+cva2))
      znames = c("Conditional Drawdown at Risk")
      if(column == 1) {
        resultingtable = data.frame(Value = z, row.names = znames)
      }
      else {
        nextcolumn = data.frame(Value = z, row.names = znames)
        resultingtable = cbind(resultingtable, nextcolumn)
      }
      
    }
    colnames(resultingtable) = columnnames
    #ans = base::round(resultingtable, digits)
    #ans
    resultingtable
  }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CDrawdown.R 2271 2012-09-02 01:56:23Z braverock $
#
###############################################################################
