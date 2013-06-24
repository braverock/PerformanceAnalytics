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
chart.PSR<-function(x,Rf,refSR,p=0.95,...){
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

