#'@title
#'Rolling Economic Drawdown Controlled Optimal Portfolio Strategy
#'
#'@description
#'The Rolling Economic Drawdown Controlled Optimal Portfolio Strategy(REDD-COPS) has 
#'the portfolio fraction allocated to single risky asset as:
#'
#' 
#' The risk free asset accounts for the rest of the portfolio allocation \eqn{x_f = 1 - x_t}.
#' 
#' For two risky assets in REDD-COPS,dynamic asset allocation weights are :
#' 
#'  
#'The portion of the risk free asset is \eqn{x_f = 1 - x_1 - x_2}.
#'dt<-read.zoo("../data/ret.csv",sep=",",header = TRUE)
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#'@param delta Drawdown limit
#'@param sharpe If you want to use a constant Sharpe Ratio please specify here
#'else the return series will be used
#'@param Rf risk free rate can be vector such as government security rate of return.
#'@param h Look back period
#'@param geometric geometric utilize geometric chaining (TRUE) or simple/arithmetic 
#'chaining(FALSE) to aggregate returns, default is TRUE.
#'@param \dots any other variable
#'@param asset The number of risky assets in the portfolio
#'@param type The type of portfolio optimization
#'
#'@author Pulkit Mehrotra
#'@seealso  \code{\link{chart.REDD}} \code{\link{EconomicDrawdown}} 
#'\code{\link{rollDrawdown}} \code{\link{EDDCOPS}} \code{\link{rollEconomicMax}}
#'@references Yang, Z. George and Zhong, Liang, Optimal Portfolio Strategy to 
#'Control Maximum Drawdown - The Case of Risk Based Dynamic Asset Allocation (February 25, 2012)
#'
#'
#'@examples
#'
#' # with S&P 500 data and T-bill data
#'dt<-data(ret)
#'dt<-as.xts(read.zoo(ret))
#'REDDCOPS(dt[,1],delta = 0.33,Rf = (1+dt[,3])^(1/12)-1,h = 12,geometric = TRUE,asset = "one")
#'
#'
#' # with S&P 500 , barclays and T-bill data
#'data(ret)
#'dt<-as.xts(read.zoo(ret))
#'REDDCOPS(dt[,1:2],delta = 0.33,Rf = (1+dt[,3])^(1/12)-1,h = 12,geometric = TRUE,asset = "two")
#'
#'data(edhec)
#'REDDCOPS(edhec,delta = 0.1,Rf = 0,h = 40)
#'data(managers)
#'REDDCOPS(managers[,1],0.80, Rf = managers[,10,drop=FALSE],12,asset="one")
#'@export
#'

REDDCOPS<-function(R ,delta,Rf,h,geometric = TRUE,asset = c("one","two","three"),type=c("calibrated","risk-based"),sharpe = 0,...){
  # DESCRIPTION
  # Calculates the dynamic weights for single and double risky asset portfolios
  # using Rolling Economic Drawdown

  # INPUT:
  # The Return series ,drawdown limit, risk free rate and the lookback period are 
  # given as the input
  
  # FUNCTION:
  x = checkData(R)
  columns = ncol(x)
  columnnames = colnames(x)
  sharpe = SharpeRatio.annualized(x,Rf)
  sd = StdDev.annualized(R)
  rho = cor(x)
  asset = asset[1]
  type = type[1]
  if(asset == "two" && columns != 2 ){
      stop("The number of series should be two")
    }
  
    if(asset == "three" && columns != 3){
    stop("The number of series should be three")
  }
  dynamicPort<-function(x,column){
    if(type == "calibrated"){
      if(asset == "one"){
        mu = mean(x)
        factor = (sharpe[,column]/sd[,column]+0.5)/(1-delta^2)
        xt = ifelse(factor*(delta-x)/(1-x)>0,factor*(delta-x)/(1-x),0)
      }
      if(asset == "two"){
        if(column == 1){
          factor = (sharpe[,1] + 0.5*sd[,1]-rho[1,1]*(sharpe[,2] + 0.5*sd[,2]))/sd[,1]
          xt = ifelse(factor*(delta-x)/(1-x)>0,factor*(delta-x)/(1-x),0)
        }
        if(column == 2){
          factor = (sharpe[,2] + 0.5*sd[,2]-rho[1,1]*(sharpe[,1] + 0.5*sd[,1]))/sd[,2]
          xt = ifelse(factor*(delta-x)/(1-x)>0,factor*(delta-x)/(1-x),0)
        }
        
      }
      if(asset == "three"){
        if(column == 1){
          factor = ((sharpe[,1] + 0.5*sd[,1])*(1-rho[2,3]^2)-(rho[2,3]*rho[1,3]-rho[1,2])*(sharpe[,2] + 0.5*sd[,2])+(rho[2,3]*rho[1,2]-rho[1,3])*(sharpe[,3] + 0.5*sd[,3]))/sd[,1]
          xt = ifelse(factor*(delta-x)/(1-x)>0,factor*(delta-x)/(1-x),0)
        }
        if(column == 2){
          factor = ((sharpe[,2] + 0.5*sd[,2])*(1-rho[1,3]^2)-(rho[1,3]*rho[2,3]-rho[1,2])*(sharpe[,1] + 0.5*sd[,1])+(rho[1,3]*rho[1,2]-rho[2,3])*(sharpe[,3] + 0.5*sd[,3]))/sd[,2]
          xt = ifelse(factor*(delta-x)/(1-x)>0,factor*(delta-x)/(1-x),0)
        }
        
        if(column == 3){
          factor = ((sharpe[,3] + 0.5*sd[,3])*(1-rho[1,2]^2)-(rho[2,3]*rho[1,2]-rho[1,3])*(sharpe[,1] + 0.5*sd[,1])+(rho[1,3]*rho[1,2]-rho[2,3])*(sharpe[,2] + 0.5*sd[,2]))/sd[,3]
          xt = ifelse(factor*(delta-x)/(1-x)>0,factor*(delta-x)/(1-x),0)
        }
        
      }
      
    }
    if(type =="risk-based"){
      
      sharpe = mean(na.omit(apply.rolling(x,width = h,FUN = SharpeRatio.annualized,Rf = 0)))
      sd = mean(na.omit(apply.rolling(x,width = h, FUN = StdDev.annualized)))
      factor = 1/(1-delta^2)
      xt = (sharpe/sd + 0.5)*ifelse(factor*(delta-x)/(1-x)>0,factor*(delta-x)/(1-x),0)
      
    }
    return(xt)
  }
  
  redd = rollDrawdown(R,Rf,h,geometric)

  for(column in 1:columns){
    column.xt <- na.skip(redd[,column],FUN = dynamicPort,column = column)
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
