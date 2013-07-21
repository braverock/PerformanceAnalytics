#'@title Benchmark Sharpe Ratio Plots
#'
#'@description
#'Benchmark Sharpe Ratio Plots are used to give the relation ship between the
#'Benchmark Sharpe Ratio and average correlation,average sharpe ratio or the number of #'strategies keeping other parameters constant. Here average Sharpe ratio , average #'correlation stand for the average of all the strategies in the portfolio. The original 
#'point of the return series is also shown on the plots. 
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#'@param ylab set the y-axis label, as in \code{\link{plot}}
#'@param xlab set the x-axis label, as in \code{\link{plot}}
#'@param main set the chart title, as in \code{\link{plot}}
#'@param element.color set the element.color value as in \code{\link{plot}}
#'@param lwd set the width of the line, as in \code{\link{plot}}
#'@param pch set the pch value, as in \code{\link{plot}}
#'@param cex set the cex value, as in \code{\link{plot}}
#'@param cex.axis set the cex.axis value, as in \code{\link{plot}}
#'@param cex.main set the cex.main value, as in \code{\link{plot}}
#'@param vs The values against which benchmark SR has to be plotted. can be 
#'"sharpe","correlation" or "strategies"
#'@param ylim set the ylim value, as in \code{\link{plot}}
#'@param xlim set the xlim value, as in \code{\link{plot}}

#'@references
#'Bailey, David H. and Lopez de Prado, Marcos, The Strategy Approval Decision: 
#'A Sharpe Ratio Indifference Curve Approach (January 2013). Algorithmic Finance, 
#'Vol. 2, No. 1 (2013).
#'
#'@seealso \code{\link{plot}}
#'@keywords ts multivariate distribution models hplot
#'@examples
#'
#'chart.BenchmarkSR(edhec,vs="strategies")
#'chart.BenchmarkSR(edhec,vs="sharpe")
#'@export

chart.BenchmarkSR<-function(R=NULL,main=NULL,ylab = NULL,xlab = NULL,element.color="darkgrey",lwd = 2,pch = 1,cex = 1,cex.axis=0.8,cex.lab = 1,cex.main = 1,vs=c("sharpe","correlation","strategies"),xlim = NULL,ylim = NULL,...){
  
  # DESCRIPTION:
  # Draws Benchmark SR vs various variables such as average sharpe , 
  # average correlation and the number of strategies
  
  # INPUT:
  # The Return Series of the portfolio is taken as the input. The Return 
  # Series can be an xts, vector, matrix, data frame, timeSeries or zoo object of
  # asset returns.
  
  # All other inputs are the same as "plot" and are principally included
  # so that some sensible defaults could be set.
  
  # vs parameter takes the value against which benchmark sr has to be plotted
  
  # FUNCTION:
  if(!is.null(R)){
    x = checkData(R)
    columns = ncol(x)
    avgSR = mean(SharpeRatio(R))
  }
  else{
    if(is.null(avgSR) | is.null(S)){
      stop("The average SR and the number of strategies should not be NULL")
    }
    
  }
  corr = table.Correlation(edhec,edhec)
  corr_avg = 0
  for(i in 1:(columns-1)){
    for(j in (i+1):columns){
      corr_avg = corr_avg + corr[(i-1)*columns+j,]
    }
  }
  corr_avg = corr_avg*2/(columns*(columns-1))
  if(vs=="sharpe"){
    if(is.null(ylab)){
      ylab = "Benchmark Sharpe Ratio"
    }
    if(is.null(xlab)){
      xlab = "Average Sharpe Ratio"
    }
    if(is.null(main)){
      main = "Benchmark Sharpe Ratio vs Average Sharpe Ratio"
    }
    sr = seq(0,1,length.out=30)
    SR_B = sr*sqrt(columns/(1+(columns-1)*corr_avg[1,1]))
    plot(sr,SR_B,type="l",xlab=xlab,ylab=ylab,main=main,lwd = lwd,pch=pch,cex = cex,cex.lab = cex.lab)
    points(avgSR,BenchmarkSR(R),col="blue",pch=10)
    text(avgSR,BenchmarkSR(R),"Return Series ",pos=4)
  } 
  if(vs=="correlation"){
    
    if(is.null(ylab)){
      ylab = "Benchmark Sharpe Ratio"
    }
    if(is.null(xlab)){
      xlab = "Average Correlation"
    }
    if(is.null(main)){
      main = "Benchmark Sharpe Ratio vs Correlation"
    }
    rho = seq(0,1,length.out=30)
    SR_B = avgSR*sqrt(columns/(1+(columns-1)*rho))
    plot(rho,SR_B,type="l",xlab=xlab,ylab=ylab,main=main,lwd = lwd,pch=pch,cex = cex,cex.lab = cex.lab)
    points(corr_avg[1,1],BenchmarkSR(R),col="blue",pch=10)
    text(corr_avg[1,1],BenchmarkSR(R),"Return Series ",pos=4)
  }
  if(vs=="strategies"){
    
    if(is.null(ylab)){
      ylab = "Benchmark Sharpe Ratio"
    }
    if(is.null(xlab)){
      xlab = "Number of Strategies"
    }
    if(is.null(main)){
      main = "Benchmark Sharpe Ratio vs Number of Strategies"
    }
    n = seq(2,100,length.out=20)
    SR_B = avgSR*sqrt(n/(1+(n-1)*corr_avg[1,1]))
    plot(n,SR_B,type="l",xlab=xlab,ylab=ylab,main=main,lwd = lwd,pch=pch,cex = cex,cex.lab = cex.lab)
    points(columns,BenchmarkSR(R),col="blue",pch=10)
    text(columns,BenchmarkSR(R),"Return Series ",pos=4)
  }

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2013 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: BenchmarkSRPlots.R $
#
###############################################################################