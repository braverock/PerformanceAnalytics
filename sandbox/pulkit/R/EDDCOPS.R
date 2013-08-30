#'@title
#'Economic Drawdown Controlled Optimal Portfolio Strategy
#'
#'@description
#'The  Economic Drawdown Controlled Optimal Portfolio Strategy(EDD-COPS) has 
#'the portfolio fraction allocated to single risky asset as:
#'
#' 
#' The risk free asset accounts for the rest of the portfolio allocation \eqn{x_f = 1 - x_t}.
#'dt<-read.zoo("../data/ret.csv",sep=",",header = TRUE)
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#'@param delta Drawdown limit
#'@param gamma (1-gamma) is the investor risk aversion
#'else the return series will be used
#'@param Rf risk free rate can be vector such as government security rate of return.
#'@param geometric geometric utilize geometric chaining (TRUE) or simple/arithmetic #'chaining(FALSE) to aggregate returns, default is TRUE.
#'@param ... any other variable
#'@author Pulkit Mehrotra
#'@seealso  \code{\link{chart.REDD}} \code{\link{EconomicDrawdown}} 
#'\code{\link{rollDrawdown}} \code{\link{REDDCOPS}} \code{\link{rollEconomicMax}}
#'
#'@references Yang, Z. George and Zhong, Liang, Optimal Portfolio Strategy to 
#'Control Maximum Drawdown - The Case of Risk Based Dynamic Asset Allocation (February 25, 2012)
#'
#'
#'@examples
#'
#' # with S&P 500 data and T-bill data
#'data(ret)
#'dt<-as.xts(read.zoo(ret))
#'EDDCOPS(dt[,1],delta = 0.33,gamma = 0.7,Rf = (1+dt[,2])^(1/12)-1,geometric = TRUE)
#'
#'data(edhec)
#'EDDCOPS(edhec,delta = 0.1,gamma = 0.7,Rf = 0)
#'@export
#'

EDDCOPS<-function(R ,delta,gamma,Rf,geometric = TRUE,...){
  # DESCRIPTION
  # Calculates the dynamic weights for single and double risky asset portfolios
  # using  Economic Drawdown
  
  # INPUT:
  # The Return series ,drawdown limit, risk aversion,risk free rate  are 
  # given as the input
  
  # FUNCTION:
  x = checkData(R)
  rf = checkData(Rf)
  columns = ncol(x)
  columnnames = colnames(x)
  sharpe = SharpeRatio.annualized(x,Rf)

  sd = StdDev.annualized(R)
  dynamicPort<-function(x){
    factor = (sharpe[,column]/sd[,column]+0.5)/(1-delta*gamma)
    xt = ifelse(factor*(delta-x)/(1-x)>0,factor*(delta-x)/(1-x),0)
    return(xt)
  }
  
  edd = EconomicDrawdown(R,Rf,geometric)
  for(column in 1:columns){
    column.xt <- na.omit(edd[,column],FUN = dynamicPort,column = column)
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
# $Id: EDDCOPS.R $
#
##############################################################################
