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

ProbSharpeRatio<-
function(R, refSR,Rf=0,p = 0.95, weights = NULL, ...){
    x = checkData(R)
    columns = ncol(R)
    columnnames = colnames(R)
    
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)
    
    psr <- function (x,Rf,p,refSR,...){
        sr = SharpeRatio(x, Rf, p,"StdDev")
        n = nrow(x)
        sd = StdDev(x)
        sk = skewness(x)
        kr = kurtosis(x)
        PSR = pnorm(((sr - refSR)*(n-1)^(0.5))/(1-sr*sk+sr^2*(kr-1)/4)^(0.5))
        return(PSR)
}

mintrl <- function(x,Rf,p,refSR,...){
    sk = skewness(x)
    kr =kurtosis(x)
    sr = SharpeRatio(x, Rf, p, "StdDev")
    MinTRL = 1 + (1 - sk*sr + ((kr-1)/4)*sr^2)*(qnorm(p)/(sr-refSR))^2
    return(MinTRL)

}
    for(column in 1:columns){
      column.probsharpe <- psr(x[,column],Rf,p,refSR)
      column.mintrack <- mintrl(x[,column],Rf,p,refSR)
      if(column == 1){
        probsharpe = column.probsharpe
        mintrack = column.mintrack
      }
      else {
        probsharpe = merge(probsharpe, column.probsharpe)
        mintrack = merge(mintrack, column.mintrack)
    }
      
    }
    
    probsharpe = rbind(probsharpe,mintrack)
    
    colnames(probsharpe) = columnnames
    probsharpe = reclass(probsharpe, x)
    rownames(probsharpe)=c("PSR","MinTRL")
    return(probsharpe)

}

