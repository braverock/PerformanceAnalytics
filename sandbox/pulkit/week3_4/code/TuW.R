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

TuW<-function(R,confidence,type=c("ar","normal"),...){
  x = checkData(R)
  type = type[1] 
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
        if(type=="ar"){
            result = get_TuW(x,confidence)
        }
        if(type=="normal"){
            result = tuw_norm(x,confidence)
        }
    }
    return(result)
  }
    else{
        if(type=="ar"){
            result=apply(x,MARGIN = 2, get_TuW,confidence)
        }
        if(type=="normal"){
             result=apply(x,MARGIN = 2, tuw_norm,confidence)
        }
                   
      result<-as.data.frame(result)
      result<-t(result)
      rownames(result)=paste("Max Time Under Water")
      return(result)
    }
    
  }
      

