#' @title
#' Triple Penance Rule
#'
#' @description
#' \code{MaxDD} calculates the Maximum drawdown for a particular confidence interval.
#' Maximum Drawdown tells us Up to how much could a particular strategy lose with 
#' a given confidence level ?. This function calculated Maximum Drawdown for two
#' underlying processes normal and autoregressive. For a normal process 
#' Maximum Drawdown is given by the formula
#' When the distibution is normal
#' 
#' \deqn{MaxDD_\alpha=max\left\{0,\frac{(z_\alpha\sigma)^2}{4\mu}\right\}}
#' 
#' The time at which the Maximum Drawdown occurs is given by
#' \deqn{t^\ast=\biggl(\frac{Z_{\alpha}\sigma}{2\mu}\biggr)^2}
#' Here \eqn{Z_{\alpha}} is the critical value of the Standard Normal Distribution  associated with a probability \eqn{\alpha}.\eqn{\sigma} and \eqn{\mu} are the Standard Distribution and the mean respectively.
#' When the distribution is non-normal and time dependent, Autoregressive process.
#' 
#' \deqn{Q_{\alpha,t}=\frac{\phi^{(t+1)}-\phi}{\phi-1}(\triangle\pi_0-\mu)+{\mu}t+Z_{\alpha}\frac{\sigma}{|\phi-1|}\biggl(\frac{\phi^{2(t+1)}-1}{\phi^2-1}-2\frac{\phi^(t+1)-1}{\phi-1}+t+1\biggr)^{1/2}}
#' 
#' \eqn{\phi} is estimated as
#' 
#' \deqn{\hat{\phi} = Cov_0[\triangle\pi_\tau,\triangle\pi_{\tau-1}](Cov_0[\triangle\pi_{\tau-1},\triangle\pi_{\tau-1}])^{-1}}
#' 
#' and the Maximum Drawdown is given by.
#' 
#' \deqn{MaxDD_{\alpha}=max\left\{0,-MinQ_\alpha\right\}}
#'
#'The non normal time dependent process is defined by
#'
#'\deqn{\triangle{\pi_{\tau}}=(1-\phi)\mu + \phi{\delta_{\tau-1}} + \sigma{\epsilon_{\tau}}}
#'
#'The random shocks are iid distributed \eqn{\epsilon_{\tau}~N(0,1)}. These random shocks follow an independent and 
#'identically distributed Gaussian Process, however \eqn{\triangle{\pi_\tau}} is neither an independent nor an 
#'identically distributed Gaussian Process. This is due to the parameter \eqn{\phi}, which incorporates a first-order serial-correlation effect of auto-regressive form.
#' 
#' Golden Section Algorithm is used to calculate the Minimum of the function Q.
#'  
#' @param R Returns
#' @param confidence the confidence interval
#' @param type The type of distribution "normal" or "ar"."ar" stands for Autoregressive.
#' @author Pulkit Mehrotra
#' @seealso  \code{\link{chart.Penance}} \code{\link{table.Penance}} \code{\link{TuW}}
#' @references Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).
#' 
#' @examples
#' 
#' data(edhec)
#' MaxDD(edhec,0.95,"ar")
#' MaxDD(edhec[,1],0.95,"normal") #expected values 4.241799 6.618966
#'@export
MaxDD<-function(R,confidence,type=c("ar","normal"),...)
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


