#'@title Probabilistic Sharpe Ratio
#'
#'@description
#'A table to display the Probabilistic Sharpe Ratio Along with 
#'the Minimum Track Record Length for better assessment of the returns.
#'
#'@aliases table.PSR
#'
#'@param R the return series
#'@param Rf the risk free rate of return
#'@param refSR the reference Sharpe Ratio
#'@param the confidence level
#'@param weights the weights for the portfolio
#'
#'@references Bailey, David H. and Lopez de Prado, Marcos, \emph{The Sharpe Ratio 
#'Efficient Frontier} (July 1, 2012). Journal of Risk, Vol. 15, No. 2, Winter
#' 2012/13
#'@keywords ts multivariate distribution models
#'@examples
#'
#'data(edhec)
#'table.PSR(edhec[,1],0.20)
#'
table.PSR<-function(R=NULL,refSR,Rf=0,p=0.95,weights = NULL,...){
    
    if(!is.null(R)){
      x = checkData(R)
      columns = ncol(x)
      n = nrow(x)
      #Checking if the weights are provided or not
      if(!is.null(weights)){
        if(length(weights)!=columns){
          stop("number of items in weights is not equal to the number of columns in R")
        }
        else{
          # A potfolio is constructed by applying the weights
          x = Return.portfolio(R,weights)
        }
      }

    }
    else{
        stop("Returns series not provided")
    }
      
      columnnames = colnames(x)
    
    for(column in 1:columns){
      column.probsharpe <- ProbSharpeRatio(x[,column],refSR,Rf,p,weights)
      column.mintrack <- MinTrackRecord(x[,column],refSR,Rf,p,weights)
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

