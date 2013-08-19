#'@title 
#'Rolling Economic Max
#'
#'@description
#'Rolling Economic Max at time t, looking back at portfolio Wealth history
#'for a rolling window of length H is given by:
#'
#'\deqn{REM(t,h)=\max_{t-H \leq s}\[(1+r_f)^{t-s}W_s\]}
#'
#'Here rf is the average realized risk free rate over a period of length t-s. If the risk free rate is changing. This is used to compound.
#'
#'\deqn{ \prod_{i=s}^{t}(1+r_{i}.{\triangle}t)}
#'
#'here  \eqn{r_i} denotes the risk free interest rate during \eqn{i^{th}} discrete 
#'time interval \eqn{{\triangle}t}.

#'
#'@param R R an xts, vector, matrix, data frame, timeseries, or zoo object of asset return.
#'@param Rf risk free rate can be vector such as government security rate of return.
#'@param h Look back period
#'@param geomtric geometric utilize geometric chaining (TRUE) or simple/arithmetic #'chaining(FALSE) to aggregate returns, default is TRUE.
#'@param ... any other variable
#'@author Pulkit Mehrotra
#'@examples
#'rollEconomicMax(edhec,0.08,100)
#'@export
#'
rollEconomicMax<-function(R,Rf,h,geometric = TRUE,...){
  
  
  # DESCRIPTION:
  # calculates the Rolling Economic Max(REDD) for a return series.
  # The risk free return(rf) and the lookback period(h) is taken as the input.
  
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
  
  REM<-function(x,geometric){
    if(geometric)
      Return.cumulative = cumprod(1+x)
    else Return.cumulative = 1 + cumsum(x)
    l = length(Return.cumulative)
    if(nr == 1){
      REM = max(Return.cumulative*(1+rf)^(l-c(1:l)))
    }
    else{
      prodRf = prod(1+rf)
      REM = max(Return.cumulative*as.numeric(last(prodRf)/prodRf))
    }
    result = REM
  }
  
  for(column in 1:columns){
    column.drawdown <- apply.rolling(x[,column],width = h, FUN = REM, geometric = geometric)
    if(column == 1)
      rolldrawdown = column.drawdown
    else rolldrawdown = merge(rolldrawdown, column.drawdown) 
  }
  colnames(rolldrawdown) = columnnames
  rolldrawdown = reclass(rolldrawdown, x)
  return(rolldrawdown)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: REM.R $
#
##############################################################################

  
