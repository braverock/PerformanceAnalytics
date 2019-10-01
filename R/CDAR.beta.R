#'@title
#'Conditional Drawdown beta
#' 
#'@description
#'The conditional drawdown beta is a measure of capturing performance under market drawdowns and it is given by
#'the ratio of  the average rate of return of the instrument over time periods corresponding to the \eqn{(1-p)T} 
#'largest drawdowns of the benchmark portfolio.
#'
#'The difference in CDaR and standard beta boils down to the fact that the standard beta accounts 
#'for the fund returns over the whole return history, including the upside 
#'while CDaR beta focuses only on market drawdowns.
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of 
#'asset returns
#'@param Rm an xts, vector, matrix, data frame, timeSeries or zoo object of benchmark returns
#'@param p confidence level for calculation ,default(p=0.95)
#'@param weights portfolio weighting vector, default NULL, see Details
#'@param geometric utilize geometric chaining (TRUE) or simple/arithmetic 
#'chaining (FALSE) to aggregate returns, default TRUE
#'@param type (Optional) Overrides the p parameter. If "average" then p = 0 and if "max" then p = 1
#'@param \dots any passthru variable.
#'@author Tasos Grivas <tasos@@openriskcalculator.com>,Pulkit Mehrotra
#' @seealso   \code{\link{CDaR.alpha}}  \code{\link{CDaR}}

#'
#'@references
#'
#'Zabarankin, M., Pavlikov, K., and S. Uryasev. Capital Asset Pricing Model 
#'(CAPM) with Drawdown Measure.Research Report 2012-9, ISE Dept., University 
#'of Florida,September 2012.
#'
#'@examples
#'data(edhec)
#'CDaR.beta(edhec[,1],edhec[,2]) 
#'CDaR.beta(edhec[,1],edhec[,2],type="max")
#'CDaR.beta(edhec[,1],edhec[,2],type="average")
#'@export
CDaR.beta<-function(R,Rm,p=0.95,weights=NULL,geometric=TRUE,type=NULL,...){
  
  type = type[1]
  if(!is.null(type))
  {
    if(type=="average"){
      p = 0
    }
    if(type == "max"){
      p = 1
    }
  }
  p= 1-p
  
  R = checkData(R)
  Rm = checkData(Rm)
  R = na.omit(R)
  Rm = na.omit(Rm)
  
  if(nrow(R) != nrow(Rm)){
    stop("The number of rows of the return series and the benchmark portfolio should be equal")
  }
  
  drawdowns_Rm = sortDrawdowns(findDrawdowns(Rm))
  
  DDbeta<-function(R){
    q_quantile = quantile(drawdowns_Rm$return,p)
    drawdowns_under_quantile_ind = which(drawdowns_Rm$return<=q_quantile)
    sum_dd_R = 0
    for(i in 1:length(drawdowns_under_quantile_ind))
    {      
      temp_R = R[drawdowns_Rm$from[drawdowns_under_quantile_ind[i]]:drawdowns_Rm$trough[drawdowns_under_quantile_ind[i]]]
      if(geometric){
        cumul_R = prod(temp_R+1)-1
      }else{
        cumul_R = sum(temp_R)
      }
      sum_dd_R = sum_dd_R + cumul_R  
    }
    if(!is.null(type))
    {
      if(type=="average"){
        CDD_value = mean(drawdowns_Rm$return)
        names(CDD_value) = 'average'
      }
      if(type == "max"){
        CDD_value = min(drawdowns_Rm$return)
        names(CDD_value) = 'max'
      }
    }else
      CDD_value = CDD(Rm,p=p, geometric=geometric, invert=FALSE)
    
    beta_dd = sum_dd_R/(length(drawdowns_under_quantile_ind)*CDD_value)
    return(beta_dd)
  }
  
  if (is.vector(R) || ncol(R)==1) {
    result = DDbeta(R)
  }else {
    if(is.null(weights)) {
      result=matrix(nrow=1,ncol=ncol(R))
      for(i in 1:ncol(R) ) {
        result[i]<-DDbeta(R[,i,drop=FALSE])
      }
      dim(result) = c(1,NCOL(R))
      colnames(result) = colnames(R)
      rownames(result) = paste("Drawdown Beta ",p*100,"%",sep='')
    } else {
      # we have weights, do the portfolio calc
      portret<-Return.portfolio(R,weights=weights,geometric=geometric)
      result<- DDbeta(portret)
    }
  }
  return(result)
}