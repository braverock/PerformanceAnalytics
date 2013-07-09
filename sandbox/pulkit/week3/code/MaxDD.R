
#' @title
#' Triple Penance Rule
#'
#' @description
#' \code{MaxDD} calculates the Maximum drawdown for a particular confidence interval.
#'  
#' @param R Returns
#' @param confidence the confidence interval
#' 
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).

MaxDD<-function(R,confidence,...)
{
    x = checkData(R)
    columns = ncol(x)
    i = 0 
    tp = matrix(nrow=columns,ncol=2)


     for(i in 1:columns){
        column_MinQ <- get_minq(x[,i],confidence)
        tp[i,] <- column_MinQ
    } 
 row.names(tp)<-colnames(R)
  colnames(tp) = c("MaxDD(in %)","t*")
  return(tp)
  
}


