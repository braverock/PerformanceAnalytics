
#' Calculates Conditional Expected Drawdown (CED)
#' 
#' Defined as the tail mean of a distribution of maximum drawdowns
#' Analagous to ETL, but accounts for autocorrelation
#' Convex and supports risk attribution, but isn't monetary
#' Calculated from the distribution of the rolling maximum drawdowns
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param p confidence level for calculation, default p=.95
#' @return 
#' @references Goldberg, L. and Mahmoud, O. (2014). On a Convex Measure of Drawdown Risk. Available at SSRN: http://ssrn.com/abstract=2430918

CED <- function(R, p=0.95, ...){
  # @TODO this is an interior function only; add multi-col support
  # Rolling 12-month max DD
  x.rMDD = NULL # initialize rolling maximum drawdown results
  for(i in colnames(x.R)){
    x.rMDD1 <- rollapply(na.omit(x.R[,i]), width = 12, align="right", FUN=maxDrawdown)
    x.rMDD = cbind(x.rMDD, x.rMDD1)
  } 
  x.qrMDD=apply(x.rMDD["1998::",], MARGIN=2, FUN=quantile, probs=0.90, na.rm=TRUE) # this is the quantile
  x.CED = NULL # Calculate the CED from the rolling MDD obs > quantile MDD
  for(i in 1:NCOL(x.R)){
    .CED = mean(x.rMDD[x.rMDD[,i] > x.qrMDD[i], i])
    x.CED = c(x.CED, .CED)
  }
  return(.CED)
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