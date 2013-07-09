#' @title
#' Time Under Water
#'
#' @description
#' \code{TriplePenance} calculates the maximum 
#' Time under water for a particular confidence interval. 
#'
#' @param R return series
#' @param confidence the confidence interval
#' 
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).

TuW<-function(R,confidence,...){
    x = checkData(R)
    columns = ncol(R)
    i = 0 
    tp = matrix(nrow=columns)
    for(i in 1:columns){
        column_TuW = get_TuW(x[,i],confidence)
        tp[i] <- column_TuW
    }


rownames(tp)<-colnames(R)
colnames(tp)<-"Max Time Under Water"
return(tp)
}
      

