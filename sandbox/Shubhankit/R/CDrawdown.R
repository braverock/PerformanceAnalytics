#' @title Chekhlov Conditional Drawdown at Risk
#' 
#' @description  A new one-parameter family of risk measures called Conditional Drawdown (CDD) has
#'been proposed. These measures of risk are functionals of the portfolio drawdown (underwater) curve considered in active portfolio management. For some value of the tolerance
#' parameter, in the case of a single sample path, drawdown functional is defineed as
#'the mean of the worst (1 - \eqn{\alpha})% drawdowns. 
#'@details The CDD measure generalizes the notion of the drawdown functional to a multi-scenario case and can be considered as a
#'generalization of deviation measure to a dynamic case. The CDD measure includes the
#'Maximal Drawdown and Average Drawdown as its limiting cases. The model is focused on concept of drawdown measure which is in possession of all properties of a deviation measure,generalization of deviation measures to a dynamic case.Concept of risk profiling - Mixed Conditional Drawdown (generalization of CDD).Optimization techniques for CDD computation - reduction to linear programming (LP) problem. Portfolio optimization with constraint on Mixed CDD
#' The model develops concept of drawdown measure by generalizing the notion
#' of the CDD to the case of several sample paths for portfolio uncompounded rate
#' of return.
#' @param Ra return vector of the portfolio
#' @param p confidence interval
#' @author Peter Carl, Brian Peterson, Shubhankit Mohan
#' @references DRAWDOWN MEASURE IN PORTFOLIO OPTIMIZATION,\emph{International Journal of Theoretical and Applied Finance}
#' ,Fall 1994, 49-58.Vol. 8, No. 1 (2005) 13-58
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
