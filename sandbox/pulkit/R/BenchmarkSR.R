#'@title 
#'Benchmark Sharpe Ratio
#'
#'@description
#'The benchmark SR is a linear function of the average annualized
#' SR of the individual strategies, and a decreasing
#' convex function of the number of strategies and the 
#' average pairwise correlation. The Returns are given as
#' the input with the benchmark Sharpe Ratio as the output.
#' 
#'\deqn{SR_B = \bar{SR}\sqrt{\frac{S}{1+(S-1)\bar{\rho}}}}
#'
#'Here \eqn{\bar{SR}} is the average annualized Sharpe Ratio of the portfolio and \eqn{\bar{\rho}} 
#'is the average correlation across off-diagonal elements.
#' 
#'
#'@param R a vector, matrix, data frame,timeseries or zoo object of asset returns
#'
#'@author Pulkit Mehrotra
#'@references
#'Bailey, David H. and Lopez de Prado, Marcos, The Strategy Approval Decision: 
#'A Sharpe Ratio Indifference Curve Approach (January 2013). Algorithmic Finance, 
#'Vol. 2, No. 1 (2013).
#'
#'@examples
#'
#'data(edhec)
#'BenchmarkSR(edhec) #expected 0.393797
#'data(managers)
#'BenchmarkSR(managers) # expected 0.8110536
#'
#'@export
#'
BenchmarkSR<-function(R){
  # DESCRIPTION:
  # Returns the Value of the Benchmark Sharpe Ratio.
  
  # INPUT:
  # The return series of all the series in the portfolio is taken as the input
  # The return series can be a vector, matrix, data frame,timeseries or zoo 
  # object of asset returns.
  
  # FUNCTION:
  x = checkData(R)
  columns = ncol(x)
  #TODO : What to do if the number of columns is only one ?  
  if(columns == 1){
    stop("The number of return series should be greater than 1")
  }
  SR = SharpeRatio.annualized(x)
  sr_avg = mean(SR)
  corr = table.Correlation(R,R)
  corr_avg = 0
  for(i in 1:(columns-1)){
    for(j in (i+1):columns){
      corr_avg = corr_avg + corr[(i-1)*columns+j,1]
    }
  }
  corr_avg = corr_avg*2/(columns*(columns-1))
  SR_Benchmark = sr_avg*sqrt(columns/(1+(columns-1)*corr_avg))
  return(SR_Benchmark)
}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2013 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: BenchmarkSR.R $
#
###############################################################################
