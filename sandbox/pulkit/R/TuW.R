#' @title
#' Maximum Time Under Water
#'
#' @description
#' \code{TriplePenance} calculates the maximum 
#' Maximum Time under water for a particular confidence interval is given by
#' 
#' For a particular sequence \eqn{\left\{\pi_t\right\}}, the time under water \eqn{(TuW)} 
#' is the minimum number of observations, \eqn{t>0}, such that \eqn{\pi_{t-1}<0} and \eqn{\pi_t>0}. 
#' 
#' For a normal distribution Maximum Time Under Water is given by the following expression.
#' \deqn{MaxTuW_\alpha=\biggl(\frac{Z_\alpha{\sigma}}{\mu}\biggr)^2}
#' 
#' For a Autoregressive process the Time under water is found using the golden section algorithm.
#' 
#'The non normal time dependent process is defined by
#'
#'\deqn{\triangle{\pi_{\tau}}=(1-\phi)\mu + \phi{\delta_{\tau-1}} + \sigma{\epsilon_{\tau}}}
#'
#'The random shocks are iid distributed \eqn{\epsilon_{\tau}~N(0,1)}. These random shocks follow an independent and 
#'identically distributed Gaussian Process, however \eqn{\triangle{\pi_\tau}} is neither an independent nor an 
#'identically distributed Gaussian Process. This is due to the parameter \eqn{\phi}, which incorporates a first-order 
#'serial-correlation effect of auto-regressive form.

#'
#' @param R return series
#' @param confidence the confidence interval
#' @param type The type of distribution "normal" or "ar"."ar" stands for Autoregressive.
#' 
#' @references Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).
#' 
#' @examples
#' TuW(edhec,0.95,"ar")
#' TuW(edhec[,1],0.95,"normal") # expected value 103.2573 
#'@export

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
      

