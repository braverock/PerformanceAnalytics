#'@title Generalised Lambda Distribution  Drawdown 
#'@description When selecting a hedge fund manager, one risk measure investors often
#' consider is drawdown. How should drawdown distributions look? Carr Futures'
#' Galen Burghardt, Ryan Duncan and Lianyan Liu share some insights from their
#'research to show investors how to begin to answer this tricky question
#'@details  To simulate net asset value (NAV) series where skewness and kurtosis are zero, 
#' we draw sample returns from a lognormal return distribution. To capture skewness 
#' and kurtosis, we sample returns from a \bold{generalised \eqn{\lambda} distribution}.The values of 
#' skewness and excess kurtosis used were roughly consistent with the range of values the paper 
#' observed for commodity trading advisers in our database. The NAV series is constructed 
#' from the return series. The simulated drawdowns are then derived and used to produce 
#' the theoretical drawdown distributions. A typical run usually requires \bold{10,000} 
#' iterations to produce a smooth distribution.
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param digits  number of rounding off digits.
#' @references Burghardt, G., and L. Liu, \emph{ It's the Autocorrelation, Stupid (November 2012) Newedge
#' working paper.}
#' \url{http://www.amfmblog.com/assets/Newedge-Autocorrelation.pdf}
#' Burghardt, G., Duncan, R. and L. Liu, \emph{Deciphering drawdown}. Risk magazine, Risk management for investors, September, S16-S20, 2003. \url{http://www.risk.net/data/risk/pdf/investor/0903_risk.pdf}
#' @author Peter Carl, Brian Peterson, Shubhankit Mohan
#' @keywords Simulated Drawdown Using Brownian Motion Assumptions
#' @examples 
#' library(PerformanceAnalytics)
#' data(edhec)
#' table.normDD(edhec[1:30,1])
#' @rdname table.normDD
#' @export
table.normDD <-
  function (R,digits =4)
  {# @author 
    
    # DESCRIPTION:
    # Downside Risk Summary: Statistics and Stylized Facts
    
    # Inputs:
    # R: a regular timeseries of returns (rather than prices)
   # Output: Table of Estimated Drawdowns 
# library(gld)
    
    y = checkData(R, method = "xts")
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)
    T= nyears(y);
    n <- 1000
    dt <- 1/T;
    r0 <- 0;
    s0 <- 1;
    # for each column, do the following:
    for(column in 1:columns) {
      x = y[,column]
      mu = Return.annualized(x, scale = NA, geometric = TRUE)
      sig=StdDev.annualized(x)
      #skew = skewness(x)
      #kurt = kurtosis(x)
      r <- matrix(0,T+1,n)  # matrix to hold short rate paths
      s <- matrix(0,T+1,n)
      r[1,] <- r0  
      s[1,] <- s0
      drawdown <- matrix(0,n)
      #  return(Ed)
      data=as.numeric(x)
      # using starship model to fit lambda distribution
      lpara= starship.adaptivegrid(data,list(lcvect=(0:4)/10,ldvect=(0:4)/10))
      
      for(j in 1:n){
             r[2:(T+1),j]= rgl(T,lpara$lambda[1],lpara$lambda[2],lpara$lambda[3],lpara$lambda[4],param="fkml")
          for(i in 2:(T+1)){
          
            dr <- r[i,j] 
            s[i,j] <- (dr)
        }
            # s=     as.POSIXct(s, origin = "1960-01-01")   
       #      drawdown[j] = as.numeric(maxDrawdown(as.POSIXct(s[,j], origin = "1960-01-01") )[1])
       drawdown[j] = as.numeric(maxDrawdown(s[,j])[1])
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
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans
 #   t <- seq(0, T, dt)
  #  matplot(t, r[1,1:T], type="l", lty=1, main="Short Rate Paths", ylab="rt")
    
  }

###############################################################################
# R (http://r-project.org/) 
#
# Copyright (c) 2004-2013 
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.normDD
#
###############################################################################
