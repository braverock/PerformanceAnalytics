#'@title Minimum Track Record Length
#'
#'@description
#'The Minimum Track Record Length responds to the following question: "How long should a track record be in 
#'order to have a p-level statistical confidence that its Sharpe ratio is above a given threshold?".
#'Obviously, the main assumption is the returns will continue displaying the same statistical properties out-of-sample.
#'For example, if the input contains fifty observations and the Minimum Track Record is forty, then for the next ten observations
#'the relevant measures (sharpe ratio, skewness and kyrtosis) need to remain the same as the input so to achieve 
#'statistical significance after exactly ten time points.
#'
#'@aliases MinTrackRecord
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of the returns input 
#' @param Rf the risk free rate
#' @param refSR a single value or a vector when R is multicolumn. It defines the reference Sharpe Ratio and should be in the same periodicity as the returns (non-annualized).
#' @param p the confidence level
#' @param weights (if R is multicolumn and the underlying assets form a portfolio) the portfolio weights
#' @param n  (if R is NULL) the track record length of the returns
#' @param sr (if R is NULL) the sharpe ratio of the returns
#' @param sk (if R is NULL) the skewness of the returns
#' @param kr (if R is NULL) the kurtosis of the returns
#' @param ignore_skewness If TRUE, it ignores the effects of skewness in the calculations
#' @param ignore_kurtosis If TRUE, it ignores the effects of kurtosis in the calculations 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>, Pulkit Mehrotra
#' @return A list containing the below
#' \itemize{  \item min_TRL:       The minimum track record length value (periodicity follows R)
#'  \item IS_SR_SIGNIFICANT:       TRUE if the sharpe ratio is statistically significant, FALSE otherwise 
#'  \item num_of_extra_obs_needed: If the sharpe ratio is not statistically significant, how many more observations are needed so as to achieve this}
#' 
#'@seealso \code{\link{ProbSharpeRatio}}
#'@references Bailey, David H. and Lopez de Prado, Marcos, The Sharpe Ratio Efficient Frontier (July 1, 2012).
#' Journal of Risk, Vol. 15, No. 2, Winter 2012/13
#'@examples
#'
#' data(edhec)
#' MinTrackRecord(edhec[,1],refSR = 0.23) 
#' MinTrackRecord(refSR = 1/12^0.5,Rf = 0,p=0.95,sr = 2/12^0.5,sk=-0.72,kr=5.78,n=59)
#' 
#' ### Higher moments are data intensive, kurtosis shouldn't be used for short timeseries
#' MinTrackRecord(edhec[,1:2],refSR = c(0.28,0.24), ignore_skewness = FALSE, ignore_kurtosis = FALSE)
#' MinTrackRecord(edhec[,1:2],refSR = c(0.28,0.24), ignore_skewness = FALSE, ignore_kurtosis = TRUE)
#' MinTrackRecord(edhec[,1:2],refSR = c(0.28,0.24), ignore_skewness = TRUE, ignore_kurtosis = TRUE)
#' 
#' MinTrackRecord(edhec[,1:2],refSR = 0.26,weights = c(0.5,0.5), ignore_skewness = FALSE, ignore_kurtosis = FALSE)
#'
#'@export
#'
MinTrackRecord<-function(R = NULL, Rf=0,refSR,p = 0.95, weights = NULL,n = NULL,sr = NULL,sk = NULL, kr = NULL,ignore_skewness = FALSE, ignore_kurtosis = TRUE){
  
  num_of_cols = 1
  column_names = NULL
  #Error handling if R is not NULL
  if(!is.null(R)){
    x = checkData(R)
    num_of_cols = ncol(x)
    n = nrow(x)
    #Checking if the weights are provided or not
    if(!is.null(weights)){
      if(length(weights)!=num_of_cols){
        stop("number of items in weights is not equal to the number of columns in R")
      }
      else{
        # A potfolio is constructed by applying the weights
        x = Return.portfolio(R,weights)
      }
    }
    
    sr = SharpeRatio(x, Rf, p, "StdDev")
    sk = skewness(x)
    kr = kurtosis(x,method='moment')
    
    
    column_names = colnames(x)
    if(is.null(weights) & length(refSR)!=num_of_cols){      stop("Reference Sharpe Ratio should be given for each series")}
    
    
  }else{
    
    if(is.null(sr) | is.null(sk) | is.null(kr) | is.null(n)){
      stop("You must either pass R or the Sharpe ratio, Skewness, Kurtosis,n etc")
    }
  }
  
  if(!is.null(dim(Rf))){
    Rf = checkData(Rf)
  }
  
  if(exists("x")) num_of_cols = ncol(x)
  
  #If the Reference Sharpe Ratio is greater than the Obserred Sharpe Ratio an error is displayed
  index_of_higher_tr = which(refSR>sr)
  if(length(index_of_higher_tr)!=0){
    if(length(index_of_higher_tr)==num_of_cols){
      stop("The reference Sharpe Ratio greater than the Observed Sharpe ratio for all the timeseries")
    }
    sr = sr[-index_of_higher_tr]
    refSR = refSR[-index_of_higher_tr]
    sk = sk[-index_of_higher_tr]
    kr = kr[-index_of_higher_tr]
    column_names = column_names[-index_of_higher_tr]
    warning(paste("The Reference Sharpe Ratio greater than the Observed Sharpe Ratio for the returns of: ",column_names[index_of_higher_tr],"\n"))
    
  }
  sk = as.numeric(sk)
  kr = as.numeric(kr)
  if(ignore_skewness) sk = rep(0,num_of_cols)
  if(ignore_kurtosis) kr = rep(3,num_of_cols)
  
  result = list()
  
  TR_length = ifelse(is.null(R),n,nrow(R))
  
  min_tr = 1 + (1 - sk*sr + ((kr-1)/4)*sr^2)*(qnorm(p)/(sr-refSR))^2
  if(!is.null(dim(min_tr))){ 
    colnames(min_tr) = paste(column_names,"(SR >",round(refSR,2),")") 
    rownames(min_tr) = paste("Minimum Track Record Length (p=",round(p*100,1),"%):")
  }
  
  TR_length = ifelse(is.null(R),n,nrow(R))
  result$min_TRL = min_tr
  result$IS_SR_SIGNIFICANT = rep(TR_length,num_of_cols)>as.numeric(min_tr)
  num_of_extra_obs_needed = as.numeric(min_tr)-rep(TR_length,num_of_cols)
  num_of_extra_obs_needed[num_of_extra_obs_needed<0] = 0
  result$num_of_extra_obs_needed = ceiling(num_of_extra_obs_needed)
    
  return(result)
}
