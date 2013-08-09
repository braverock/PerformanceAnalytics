#' Lo Sharpe Ratio
#'
#' The building blocks of the Sharpe ratio-expected returns and volatilities-
#'  are unknown quantities that must be estimated statistically and are,
#' therefore, subject to estimation error.In an illustrative
#' empirical example of mutual funds and hedge funds, Andrew Lo finds that the annual Sharpe ratio for a hedge fund can be overstated by as much as 65 percent
#' because of the presence of serial correlation in monthly returns, and once
#' this serial correlation is properly taken into account, the rankings of hedge
#' funds based on Sharpe ratios can change dramatically.
#'
#' 
#' 
#' @aliases LoSharpeRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @references  \emph{The statistics of Sharpe Ratio 
#' }, Andrew Lo
#' 
#' @keywords ts multivariate distribution models
#' @examples
#' library(PerformanceAnalytics)
#' data(edhec)
#' LoSharpe(edhec)
#' @export 

LoSharpeRatio<-
  function(R = NULL,Rf=0.,q = 0., ...)
  {
columns = 1
columnnames = NULL
#Error handling if R is not NULL
if(!is.null(R)){
  x = checkData(R)
  columns = ncol(x)
  n = nrow(x)
  
  if(q==0){
    stop("AutoCorrelation Coefficient Should be greater than 0")
    
  }
  else{
    # A potfolio is constructed by applying the weights
    
    count = q
    columns = ncol(x)
    columnnames = colnames(x)
    
    # Calculate AutoCorrelation Coefficient
    for(column in 1:columns) { # for each asset passed in as R
      y = checkData(edhec[,column], method="vector", na.rm = TRUE)
      
      acflag6 = acf(y,plot=FALSE,lag.max=6)[[1]][2:7]
      LjungBox =  Box.test(y,type="Ljung-Box",lag=q)
      values = c(acflag6, LjungBox$p.value)
      # values = base::round(as.numeric(values),digits)
      
      if(column == 1) {
        result.df = data.frame(Value = values)
        colnames(result.df) = columnnames[column]
      }
      else {
        nextcol = data.frame(Value = values)
        colnames(nextcol) = columnnames[column]
        result.df = cbind(result.df, nextcol)
      }
    }
    # Calculate Neta's
    for(column in 1:columns) {
      sum = 0
      z = checkData(edhec[,column], method="vector", na.rm = TRUE)
    for(q in 1:(q-1) )
    {
      sum = sum + (count-q)*result.df[column,q]
    
    }
      
      netaq = count/(sqrt(count+2*sum))
      if(column == 1) {
        netacol = data.frame(Value = netaq)
        colnames(netacol) = columnnames[column]
      }
      else {
          nextcol = data.frame(Value = netaq)
        colnames(nextcol) = columnnames[column]
        netacol = cbind(netacol, nextcol)
      }
      
    }
    shrp = SharpeRatio(x, Rf, FUN="VaR" , method="gaussian")
    results = Shrp*netacol
    colnames(results) = colnames(x)
    return(results)
  }
  }  
}


###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: LoSharpeRatio.R 2271 2012-09-02 01:56:23Z braverock $
#
###############################################################################
