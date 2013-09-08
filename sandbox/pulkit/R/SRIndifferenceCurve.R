#'@title
#'Sharpe Ratio Indifference Curve
#'
#'@description
#'The trade-off between a candidate's SR and its correlation
#'to the existing set of strategies, is given by the Sharpe 
#'ratio indifference curve. It is a plot between the candidate's 
#'Sharpe Ratio and candidate's average correlation for a given 
#'portfolio Sharpe Ratio.Portfolio's sharpe Ratio remains constant 
#'if any strategy from the Sharpe Ratio Indifference Curve is added.
#' 
#'The equation for the candidate's average autocorrelation for a given 
#'sharpe Ratio is given by
#'
#'\deqn{\bar{\rho{_s+1}}=\frac{1}{2}\biggl[\frac{\bar{SR}.S+SR_{s+1}^2}{S.SR_B^2}-\frac{S+1}{S}-\bar{rho}{S-1}\biggr]}
#'
#'This is the correlation that the candidate's strategy should have with the portfolio
#'for the Sharpe Ratio of the portfolio to remain constant if this strategy is added.
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#'@param reference.grid if true, draws a grid aligned with the points on the x
#'and y axes
#'@param ylab set the y-axis label, as in \code{\link{plot}}
#'@param xlab set the x-axis label, as in \code{\link{plot}}
#'@param main set the chart title, as in \code{\link{plot}}
#'@param element.color set the element.color value as in \code{\link{plot}}
#'@param lwd set the width of the line, as in \code{\link{plot}}
#'@param pch set the pch value, as in \code{\link{plot}}
#'@param cex set the cex value, as in \code{\link{plot}}
#'@param cex.lab set the cex.lab value, as in \code{\link{plot}}
#'@param cex.axis set the cex.axis value, as in \code{\link{plot}}
#'@param cex.main set the cex.main value, as in \code{\link{plot}}
#'@param ylim set the ylim value, as in \code{\link{plot}}
#'@param xlim set the xlim value, as in \code{\link{plot}}
#'@param \dots Any other passthru variable
#'@author Pulkit Mehrotra
#'@references 
#'Bailey, David H. and Lopez de Prado, Marcos, The Strategy Approval Decision: 
#'A Sharpe Ratio Indifference Curve Approach (January 2013). Algorithmic Finance, 
#'Vol. 2, No. 1 (2013).
#'
#'@seealso \code{\link{BenchmarkSR}} \code{\link{chart.BenchmarkSR}} \code{\link{plot}}
#'@keywords ts multivariate distribution models hplot
#'@examples
#' 
#'data(edhec)
#'chart.SRIndifference(edhec)
#'data(managers)
#'chart.SRIndifference(managers) 
#'@export 

chart.SRIndifference<-function(R,reference.grid = TRUE, ylab = NULL,xlab = NULL,main = "Sharpe Ratio Indifference Curve",element.color = "darkgrey",lwd = 2,pch = 1,cex = 1,cex.axis = 0.8,cex.lab = 1,cex.main = 1,ylim = NULL,xlim = NULL,...){
  
  # DESCRIPTION:
  # Draws the Sharpe Ratio Indifference curve, which  gives us pairs 
  # of correlation and sharpe ratio of strategies which when added
  # to the portfolio do not effect the portfolio's Sharpe Ratio.
  
  # INPUT:
  # The Return Series of the portfolio is taken as the input. The Return 
  # Series can be an xts, vector, matrix, data frame, timeSeries or zoo object of
  # asset returns.
  
  # All other inputs are the same as "plot" and are principally included
  # so that some sensible defaults could be set.
  
  # Output:
  # Draws the Sharpe Ratio Indifference Curve with some sensible defaults.
  
  # FUNCTION:
  x = checkData(R)
  columns = ncol(x)
  #TODO: What to do when the number of columns is 1 ?
  if(columns == 1){  
    stop("The number of return series should be greater 1 ")
  }
  SR = SharpeRatio(x,FUN="StdDev")
  sr_avg = mean(SR)
  corr = table.Correlation(R,R)
  corr_avg = 0
  for(i in 1:(columns-1)){
    for(j in (i+1):columns){
      corr_avg = corr_avg + corr[(i-1)*columns+j,]
    }
  }
  corr_avg = corr_avg*2/(columns*(columns-1))
  SR_B = BenchmarkSR(R)
  corr_range = seq(-1,1,length.out = 30)
  SR_i = NULL
  for(i in corr_range){
    
    SR_i  = c(SR_i,sqrt((i*2+((columns+1)/columns)+corr_avg[1,1]*(columns-1))*columns*SR_B^2)-sr_avg*columns)
  }
  if(is.null(ylab)){
    ylab = "Candidate Strategy's average correlation"
  }
  if(is.null(xlab)){
    xlab = "Candidate's Strategy's Sharpe Ratio"
  }
  plot(corr_range~SR_i,type="l",xlab = '',ylab = '',main=main,cex =cex,xlim = xlim,ylim = ylim,pch = pch,lwd = lwd)
  title(ylab = ylab,cex.lab = cex.lab)
  title(xlab = xlab,cex.lab = cex.lab)
  if(reference.grid) {
    grid(col = element.color)
    abline(h = 0, col = element.color)
    abline(v = 0, col = element.color)
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
# $Id: chart.SRIndifferenceCurve.R $
#
###############################################################################
