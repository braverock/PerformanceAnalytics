
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

MaxDD<-function(R,confidence,type=c("ar","normal"),...)
{
  x = checkData(R)
  
  if(ncol(x)==1 || is.null(R) || is.vector(R)){
    type = type[1] 
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
      if(type=="ar"){  
        result = get_minq(x,confidence)
        }
      if(type=="normal"){
          result = dd_norm(x,confidence)
      }
    }

    return(result)
  }
    if(type=="ar"){
        result = apply(x,MARGIN = 2,get_minq,confidence)
    }
    if(type=="normal"){
        result = apply(x,MARGIN = 2,dd_norm,confidence)
    }
  rownames(result) = c("MaxDD(in %)","t*")
  return(result)  
}


