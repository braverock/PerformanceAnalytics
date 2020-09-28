#' @title
#' Conditional Drawdown alpha
#'
#' @description
#' The difference between the actual rate of return and the rate of
#' return of the instrument estimated via the conditional drawdown beta 
#' is called \eqn{CDaR.alpha} and it is the equivalent of the typical CAPM 
#' alpha but focusing on market drawdowns.
#'
#' Positive \eqn{CDaR.alpha} implies that the instrument performed better than it was
#' predicted, and consequently, \eqn{CDaR.alpha} can be used as a performance
#' measure to rank instrument who overperform under market drawdowns.
#'
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#'@param Rm	an xts, vector, matrix, data frame, timeSeries or zoo object of benchmark returns
#'@param p confidence level for calculation ,default(p=0.95)
#'@param weights portfolio weighting vector, default NULL
#'@param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns, default TRUE
#'@param type (Optional) Overrides the p parameter. If "average" then p = 0 and if "max" then p = 1
#'@param \dots any passthru variable
#'
#'@author Tasos Grivas <tasos@@openriskcalculator.com>, Pulkit Mehrotra
#'@return The annualized alpha (input data are assumed to be of monthly frequency)
#' @seealso   
#'\code{\link{CDaR}} \code{\link{CDaR.beta}}

#'@references
#'Zabarankin, M., Pavlikov, K., and S. Uryasev. Capital Asset Pricing Model 
#'(CAPM) with Drawdown Measure.Research Report 2012-9, ISE Dept., University 
#'of Florida,September 2012.
#'@examples
#'data(edhec)
#'CDaR.alpha(edhec[,1],edhec[,2])
#'
#'CDaR.alpha(edhec[,1],edhec[,2],type="max")
#'
#'CDaR.alpha(edhec[,1],edhec[,2],type="average") 
#'
#'@export


CDaR.alpha<-function(R,Rm,p=0.95,weights = NULL,geometric = TRUE,type= NULL,...){

  R = na.omit(R)
  Rm = na.omit(Rm)
  R = checkData(R)
  Rm = checkData(Rm)
  
  if(nrow(R) != nrow(Rm)){
    stop("The number of rows of the return series and the optimal portfolio should be equal")
  }
  
  if (is.vector(R) || ncol(R)==1) {
    beta = CDaR.beta(R,Rm,p = p,weights=weights,geometric=geometric,type=type,...)
    
  }    
  else {
    if(is.null(weights)) {
      alpha=matrix(nrow=1,ncol=ncol(R))
      for(i in 1:ncol(R) ) {
        beta[i]<-CDaR.beta(R[,i,drop=FALSE],p=p, geometric=geometric, invert=invert, ...=...)
      }
      dim(alpha) = c(1,NCOL(R))
      colnames(alpha) = colnames(R)
      rownames(alpha) = paste("COnditional Drawdown alpha ",p*100,"%",sep='')
    } else {
      # we have weights, do the portfolio calc
      portret<-Return.portfolio(R,weights=weights,geometric=geometric)
      beta = CDaR.beta(portret,Rm,p = p,weights=weights,geometric=geometric,type=type,...)
    }
  }
  Rm_expected_annualized = (1+mean(Rm))^12-1
  R_expected_annualized  = (1+mean(R))^12-1
  alpha = R_expected_annualized - beta*Rm_expected_annualized
  return(alpha)
}