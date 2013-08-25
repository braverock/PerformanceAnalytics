#'@title Calculate the Rolling Economic Drawdown
#'
#'@description
#'\code{rollDrawdown} calculates the Rolling Economic Drawdown(REDD) for
#'a return series.To calculate the rolling economic drawdown cumulative 
#'return and rolling economic max is calculated for each point. The risk 
#'free return(rf) and the lookback period(h) is taken as the input.
#'
#'Rolling Economic Drawdown is given by the equation
#'
#'\deqn{REDD(t,h)=1-\frac{W_t}/{REM(t,H)}}
#'
#'Here REM stands for Rolling Economic Max and is the code \code{\link{rollEconomicMax}}
#' 
#'
#'@param R an xts, vector, matrix, data frame, timeseries, or zoo object of asset return.
#'@param Rf risk free rate can be vector such as government security rate of return
#'@param h lookback period 
#'@param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining(FALSE)
#'to aggregate returns, default is TRUE
#'@param \dots any other  variable
#'@author Pulkit Mehrotra
#'@seealso  \code{\link{chart.REDD}} \code{\link{EconomicDrawdown}} 
#'\code{\link{EDDCOPS}} \code{\link{REDDCOPS}} \code{\link{rollEconomicMax}}
#'@references Yang, Z. George and Zhong, Liang, Optimal Portfolio Strategy to 
#'Control Maximum Drawdown - The Case of Risk Based Dynamic Asset Allocation (February 25, 2012)
#'@examples
#'data(edhec)
#'rollDrawdown(edhec,0.08,100)
#'
#' @export
rollDrawdown<-function(R,Rf,h, geometric = TRUE,...)
{
  
  # DESCRIPTION:
  # calculates the Rolling Economic Drawdown(REDD) for
  # a return series.To calculate the rolling economic drawdown cumulative 
  # return and rolling economic max is calculated for each point. The risk 
  # free return(rf) and the lookback period(h) is taken as the input.
  
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
    
    REDD<-function(xh,geometric){
        if(geometric)
            Return.cumulative = cumprod(1+xh)
        else Return.cumulative = 1 + cumsum(xh)
        l = length(Return.cumulative)
        if(nr == 1){
          REM = max(Return.cumulative*(1+rf)^(l-c(1:l)))
        }
        else{
          rf = rf[index(xh)]
          prodRf = cumprod(1+rf)
          REM = max(Return.cumulative*as.numeric(last(prodRf)/prodRf))
        }
        #as.numeric(first(prodRf[index(xh[which(xh==max(xh))])]))
        result = 1 - last(Return.cumulative)/REM
    }

    for(column in 1:columns){
        column.drawdown <- apply.rolling(x[,column],width = h, FUN = REDD, geometric = geometric)
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
# $Id: redd.R $
#
##############################################################################






  


