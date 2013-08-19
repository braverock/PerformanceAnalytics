#'@title Calculate the Economic Drawdown
#'
#'@description
#'\code{EconomicDrawdown} calculates the  Economic Drawdown(EDD) for
#'a return series.To calculate the economic drawdown cumulative 
#'return and economic max is calculated for each point. The risk 
#'free return(rf)  is taken as the input.
#'
#'Economic Drawdown is given by the equation
#'
#'\deqn{EDD(t)=1-\frac{W_t}/{EM(t)}}
#'
#'Here EM stands for Economic Max and is the code \code{\link{EconomicMax}}
#' 
#'
#'@param R an xts, vector, matrix, data frame, timeseries, or zoo object of asset return.
#'@param Rf risk free rate can be vector such as government security rate of return 
#'@param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining(FALSE)
#'to aggregate returns, default is TRUE
#'@param \dots any other  variable
#'@author Pulkit Mehrotra
#'@seealso  \code{\link{chart.REDD}} \code{\link{EDDCOPS}} 
#'\code{\link{rollDrawdown}} \code{\link{REDDCOPS}} \code{\link{rollEconomicMax}}
#'@references Yang, Z. George and Zhong, Liang, Optimal Portfolio Strategy to 
#'Control Maximum Drawdown - The Case of Risk Based Dynamic Asset Allocation (February 25, 2012)
#'@examples
#'EconomicDrawdown(edhec,0.08,100)
#'
#' @export
EconomicDrawdown<-function(R,Rf, geometric = TRUE,...)
{
  
  # DESCRIPTION:
  # calculates the Economic Drawdown(EDD) for
  # a return series.To calculate the economic drawdown cumulative 
  # return and rolling economic max is calculated for each point. The risk 
  # free return(rf) is taken as the input.
  
  # FUNCTION:
  x = checkData(R)
  columns = ncol(x)
  n = nrow(x)
  columnnames = colnames(x)
  rf = checkData(Rf)
  nr = length(Rf)
  if(nr != 1 && nr != n ){
    stop("The number of rows of the returns and the risk free rate do not match")
  }
  
  EDD<-function(xh,geometric){
    if(geometric)
      Return.cumulative = cumprod(1+xh)
    else Return.cumulative = 1 + cumsum(xh)
    l = length(Return.cumulative)
    if(nr == 1){
      EM = max(Return.cumulative*(1+rf)^(l-c(1:l)))
    }
    else{
      rf = rf[index(xh)]
      prodRf = cumprod(1+rf)
      EM = max(Return.cumulative*as.numeric(last(prodRf)/prodRf))
    }
    result = 1 - last(Return.cumulative)/EM
  }
  
  for(column in 1:columns){
    column.drawdown <- as.xts(apply.fromstart(x[,column], FUN = EDD, geometric = geometric,gap = 1))
    if(column == 1)
      Economicdrawdown = column.drawdown
    else Economicdrawdown = merge(Economicdrawdown, column.drawdown) 
  }
  colnames(Economicdrawdown) = columnnames
  #Economicdrawdown = reclass(Economicdrawdown, x)
  return(Economicdrawdown)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: edd.R $
#
##############################################################################
