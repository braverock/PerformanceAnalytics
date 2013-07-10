#' Expected Drawdown using Brownian Motion Assumptions
#' 
#' Works on the model specified by Maddon-Ismail
#' 
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns

#' @author R
#' @keywords Expected Drawdown Using Brownian Motion Assumptions
#'
#' @export 
chart.ArcarJames <-
  function (R,digits =4)
  {# @author 
    
    # DESCRIPTION:
    # Downside Risk Summary: Statistics and Stylized Facts
    
    # Inputs:
    # R: a regular timeseries of returns (rather than prices)
    # Output: Table of Estimated Drawdowns 
    
    y = checkData(R, method = "xts")
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)
    T= 36;
    n <- 1000
    #tlength <- 36
    musig.ratio=seq(-2,2,by=.1)
    dt <- 1/T;
    s0 <- 100;
    # for each column, do the following:
    for(ratio in 1:length(musig.ratio))
    {
    for(column in 1:columns) {
      x = y[,column]
      mu = musig.ratio[ratio] *.01
      sig= .01
      r <- matrix(0,T+1,n)  # matrix to hold stock path
      r[1,] <- s0  
      drawdown <- matrix(0,length(musig.ratio,n)
      #  return(Ed)
      
      for(j in 1:n){
        for(i in 2:(T+1)){
          
          dr <- mu*dt + sig*sqrt(dt)*rnorm(1,0,1)
          r[i,j] <- r[i-1,j] + dr
        }
        drawdown[ratio,j] = maxDrawdown(r[ratio,j])
      }
      z = c((mu*100),
            (sig*100),
            ((mean(drawdown)*100)))
      znames = c(
        "Annual Returns in %",
        "Std Devetions in %",
        "Normalized Drawdown Drawdown in %"
      )
      if(column == 1) {
        resultingtable = data.frame(Value = z, row.names = znames)
      }
      else {
        nextcolumn = data.frame(Value = z, row.names = znames)
        resultingtable = cbind(resultingtable, nextcolumn)
      }
    }
  }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans
    t <- seq(0, T, dt)
    matplot(t, r[1,1:T], type="l", lty=1, main="Short Rate Paths", ylab="rt")
    
  }

###############################################################################
# R (http://r-project.org/) 
#
# Copyright (c) 2004-2013 
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: EMaxDDGBM
#
###############################################################################
