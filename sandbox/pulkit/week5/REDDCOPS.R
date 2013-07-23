#'@title
#'Rolling Economic Drawdown Controlled Optimal Portfolio Strategy
#'
#'@description
#'The Rolling Economic Drawdown Controlled Optimal Portfolio Strategy(REDD-COPS) has 
#'the portfolio fraction allocated to single risky asset as:
#'
#' \deqn{x_t = Max\left\{0,\biggl(\frac{\lambda/\sigma + 1/2}{1-\delta.\gamma}\biggr).\biggl[\frac{\delta-REDD(t,h)}{1-REDD(t,h)}\biggr]\right\}}
#' 
#' The risk free asset accounts for the rest of the portfolio allocation \eqn{x_f = 1 - x_t}.
#' 
#' For two risky assets in REDD-COPS,dynamic asset allocation weights are :
#' 
#' \deqn{\left[{\begin{array}{c} x_1 \\
#'                               x_2 
#'                               \end{array}}\right] = \frac{1}{1-{\rho}^2}\left[\begin{array{c} (\lambda_1 + {1/2}*\sigma_1 - \rho.(\lambda_2 + {1/2}.\sigma_2)/\sigma_1) \\
#'(\lambda_1 + {1/2}*\sigma_1 - \rho.(\lambda_2 + {1/2}.\sigma_2)/\sigma_1)
#'\end{array}}\right].Max\left\{0,\biggl(\frac{\lambda/\sigma + 1/2}{1-\delta.\gamma}\biggr).\biggl[\frac{\delta-REDD(t,h)}{1-REDD(t,h)}\biggr]\right\}}
#'  
#'The portion of the risk free asset is \eqn{x_f = 1 - x_1 - x_2}.
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#'@param delta Drawdown limit
#'@param sharpe If you want to use a constant Sharpe Ratio please specify here
#'else the return series will be used
#'@param Rf risk free rate can be vector such as government security rate of return.
#'@param h Look back period
#'@param geomtric geometric utilize geometric chaining (TRUE) or simple/arithmetic #'chaining(FALSE) to aggregate returns, default is TRUE.
#'@param ... any other variable
#'
#'@references Yang, Z. George and Zhong, Liang, Optimal Portfolio Strategy to 
#'Control Maximum Drawdown - The Case of Risk Based Dynamic Asset Allocation (February 25, 2012)
#'
#'
#'@examples
#'REDDCOPS(edhec,delta = 0.1,Rf = 0,h = 40)
#'
#'@export
#'

REDDCOPS<-function(R ,delta,Rf,h,geometric = TRUE,sharpe=NULL,...){
  # DESCRIPTION
  # Calculates the dynamic weights for single and double risky asset portfolios
  # using Rolling Economic Drawdown

  # INPUT:
  # The Return series ,drawdown limit, risk free rate and the lookback period are 
  # given as the input
  
  # FUNCTION:
  x = checkData(R)
  columns = ncol(x)
  n = nrow(x)
  columnnames = colnames(x)
  rf = checkData(Rf)
  nr = length(Rf)

  if(is.null(sharpe)){
    sharpe = SharpeRatio(R,FUN="StdDev",Rf ,p=0.95)
  }
  dynamicPort<-function(x){
    sd = StdDev(R)
    factor = (as.vector(sharpe)/as.vector(sd)+0.5)/(1-delta^2)
    redd = rollDrawdown(R,Rf,h,geometric)
    xt = max(0,(delta-redd)/(1-redd))
    return(xt)
  }
  for(column in 1:columns){
    column.xt <- as.xts(apply((x[,column],MARGIN = 1,FUN = dynamicPort)))
    if(column == 1)
      xt = column.xt
    else xt = merge(xt, column.xt) 
  }
  colnames(xt) = columnnames
  xt = reclass(xt, x)
  return(xt)
  
}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: REDDCOPS.R $
#
##############################################################################
