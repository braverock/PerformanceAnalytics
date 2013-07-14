
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
  
  if(ncol(x)==1 || is.null(R) || is.vector(R)){
    
    calcul = FALSE
    for(i in (1:length(x))){
      if(!is.na(x[i])){
        calcul = TRUE
      }
    }
    x = na.omit(x)
    if(!calcul){
      result = NA
    }
    else{
      result = get_minq(x,confidence)
    }
    return(result)
  }
    
    result = apply(x,MARGIN = 2,get_minq,confidence)
  rownames(result) = c("MaxDD(in %)","t*")
  return(result)  
}


