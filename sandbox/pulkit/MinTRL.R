#'@title Probabilistic Sharpe Ratio
#'@description
#'Given a predefined
#'benchmark4 Sharpe ratio (), the observed Sharpe  Ratiô can be expressed
#' in probabilistic
#'
#'@param R the return series
#'@param Rf the risk free rate of return
#'@param refSR the reference Sharpe Ratio
#'@param the confidence level
#'@param weights the weights for the portfolio

MinTrackRecord<-function(x,Rf,refSR,p=0.95,...){

mintrl <- function(x,Rf,p,refSR,...){
    sk = skewness(x)
    kr =kurtosis(x)
    sr = SharpeRatio(x, Rf, p, "StdDev")
    MinTRL = 1 + (1 - sk*sr + ((kr-1)/4)*sr^2)*(qnorm(p)/(sr-refSR))^2
    return(MinTRL)

}
    for(column in 1:columns){
      column.mintrack <- mintrl(x[,column],Rf,p,refSR)
      if(column == 1){
        mintrack = column.mintrack
      }
      else {
        mintrack = merge(mintrack, column.mintrack)
    }
      
    }
}

