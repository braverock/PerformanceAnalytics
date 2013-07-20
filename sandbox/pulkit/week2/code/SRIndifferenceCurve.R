#'@title
#'Sharpe Ratio Indifference Curve
#'
#'@description
#'The trade-off between a candidate’s SR and its correlation
#' to the existing set of strategies, is given by the Sharpe 
#' ratio indifference curve. It is a plot between the candidate's 
#' Sharpe Ratio and candidate's average correlation for a given 
#' portfolio Sharpe Ratio.
#' 
#'The equation for the candidate's average autocorrelation for a given 
#'sharpe Ratio is given by
#'
#'\deqn{\bar{\rho{_s+1}}=\frac{1}{2}\biggl[\frac{\bar{SR}.S+SR_{s+1}^2}{S.SR_B^2}-\frac{S+1}{S}-\bar{rho}{S-1}\biggr]}
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#'@param ylab set the y-axis label, as in \code{\link{plot}}
#'@param xlab set the x-axis label, as in \code{\link{plot}}
#'@param lwd set the width of the line, as in \code{\link{plot}}
#'@param pch set the pch value, as in \code{\link{plot}}
#'@param cex set the cex value, as in \code{\link{plot}}
#'
#'@references 
#'Bailey, David H. and Lopez de Prado, Marcos, The Strategy Approval Decision: 
#'A Sharpe Ratio Indifference Curve Approach (January 2013). Algorithmic Finance, 
#'Vol. 2, No. 1 (2013).
#'
#'
SRIndifference<-function(R, ylab = NULL,xlab = NULL,lwd = 2,pch = 1,cex = 1,...){
  
  x = checkData(R)
  columns = ncol(x)
  #TODO: What to do when the number of columns is 1 ?
  if(columns == 1){  
    stop("The number of return series should be greater 1 ")
  }
  SR = SharpeRatio(x)
  sr_avg = mean(SR)
  corr = table.Correlation(edhec,edhec)
  corr_avg = 0
  for(i in 1:(columns-1)){
    for(j in (i+1):columns){
      corr_avg = corr_avg + corr[(i-1)*columns+j,]
    }
  }
  corr_avg = corr_avg*2/(columns*(columns-1))
  SR_B = BenchmanrkSR(R)
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
  plot(SR_i,corr_range,type="l",xlab = xlab,ylab = ylab,main="Sharpe Ratio Indifference Curve")
  #OR we can use ggplot2 for much better plots
  #qplot(SR_i,corr_range,geom="line",xlab=xlab,ylab=ylab,main="Sharpe Ratio IndifferenceCurve",margins=TRUE,facet="grid")+stat_summary()
}
