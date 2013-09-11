#' @title
#' Penance
#'
#'@description
#'A plot for Penance vs phi for the given portfolio
#'The relationship between penance and phi is given by
#'
#'\deqn{penance = \frac{Maximum Time under water}{t_\alpha^{*}-1}}
#'
#'Penance Measures how long it takes to recover from the maximum drawdown
#'as a multiple of the time it took to reach the bottom. Penance is smaller,
#'the higher the value of \eqn{\phi(Phi)} and the higher the ratio \eqn{\frac{\mu}{\sigma}}.
#'Positive serial autocorrelation leads to smaller Penance due to greater periods under 
#'water.
#' @param R Returns
#' @param confidence the confidence interval
#' @param type The type of distribution "normal" or "ar"."ar" stands for Autoregressive.
#' @param \dots any other passthru variable
#' @author Pulkit Mehrotra
#' @seealso  \code{\link{chart.Penance}} \code{\link{table.Penance}} \code{\link{TuW}}
#' @references Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the "Triple Penance" Rule(January 1, 2013).
#' 
#' @examples
#' data(edhec)
#' Penance(edhec,0.95,"ar")
#' Penance(edhec[,1],0.95,"normal") 
#'@export
Penance<-function(R,confidence=0.95,type=c("ar","normal"),...)
{
  
  # DESCRIPTION:
  # Calculates the maximum drawdown for the return series based on the given 
  # distribution normal or autoregressive.
  
  # INPUT:
  # The Return Series of the portfolio is taken as the input. The Return 
  # Series can be an xts, vector, matrix, data frame, timeSeries or zoo object of
  # asset returns. The type of distribution , "normal" or non-normal "ar", The confidence 
  # level
  
  # FUNCTION:
  x = checkData(R)
  if(ncol(x)==1 || is.null(R) || is.vector(R)){
    calcul = FALSE
    for(i in (1:length(x))){
      if(!is.na(x[i])){
        calcul = TRUE
      }
    }
    if(!calcul){
      result = NA
    }
    else{
      if(type[1]=="ar"){  
        result = get_penance(x,confidence)
        }
      if(type[1]=="normal"){
          result = penance_norm(x,confidence)
      }
    }

    return(result)
  }
    if(type[1]=="ar"){
        result = apply(x,MARGIN = 2,get_penance,confidence)
    }
    if(type[1]=="normal"){
        result = apply(x,MARGIN = 2,penance_norm,confidence)
    }
    result = round(result,3)
    result = as.data.frame(result)
    result = t(result)
  rownames(result) = paste("Penance")
  return(result)  
}


