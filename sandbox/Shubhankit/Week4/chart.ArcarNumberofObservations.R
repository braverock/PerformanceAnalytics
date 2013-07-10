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
      for(column in 1:columns) {
        x = y[,column]
        mu = musig.ratio[ratio] *.01
        sig= .01
        r <- matrix(0,T+1,n)  # matrix to hold Asset Path
        r[1,] <- s0  
        drawdown <- matrix(0,length(musig.ratio),n)
        # Generate k random walks across time {0, 1, ... , T}
        T <- 100
        k <- 250
        initial.value <- 100
        GetRandomWalk <- function() {
          # Add a standard normal at each step
          initial.value + c(0, cumsum(rnorm(T)))
        
        }
        # Matrix of random walks
        values <- replicate(k, GetRandomWalk())
        # Create an empty plot
        dev.new(height=8, width=12)
        plot(0:T, rep(NA, T + 1), main=sprintf("%s Random Walks", k),
             xlab="time", ylab="value",
             ylim=100 + 4.5 * c(-1, 1) * sqrt(T))
        mtext(sprintf("%s%s} with initial value of %s",
                      "Across time {0, 1, ... , ", T, initial.value))
        for (i in 1:k) {
          lines(0:T, values[ , i], lwd=0.25)
        }
        for (sign in c(-1, 1)) {
          curve(initial.value + sign * 1.96 * sqrt(x), from=0, to=T,
                n=2*T, col="darkred", lty=2, lwd=1.5, add=TRUE)
        }
        legend("topright", "1.96 * sqrt(t)",
               bty="n", lwd=1.5, lty=2, col="darkred")
        
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
# $Id: chart.ArcarNumberofObservations
#
###############################################################################
