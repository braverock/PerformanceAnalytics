#' @title Downside Sharpe Ratio
#' 
#' @details 
#' 
#' The Downside Sharpe Ratio (DSR) is a short name for what Ziemba (2005) called the "Symmetric Downside Risk Sharpe Ratio" and is
#' defined as the ratio of the mean excess return to the square root of lower semivariance:
#'  
#'  \deqn{\frac{\overline{(R_{a}-R_{f})}}{\sqrt{2}SemiSD(R_a)}}.
#'
#' @description \code{DownsideSharpeRatio} computation with standard errors
#'
#' @param R Data of returns for one or multiple assets or portfolios.
#' @param rf Risk-free interest rate.
#' @param SE TRUE/FALSE whether to ouput the standard errors of the estimates of the risk measures, default FALSE.
#' @param SE.control Control parameters for the computation of standard errors. Should be done using the \code{\link{RPESE.control}} function.
#' @param ... Additional parameters.
#'
#' @return A vector or a list depending on \code{se.method}.
#'
#' @export
#'
#' @author Anthony-Alexander Christidis, \email{anthony.christidis@stat.ubc.ca}
#' 
#' @references 
#' 
#' Ziemba, W. T. (2005). The symmetric downside-risk Sharpe ratio. The Journal of Portfolio Management, 32(1), 108-122.
#'
#' @examples
#' # Loading data from PerformanceAnalytics
#' data(edhec, package = "PerformanceAnalytics")
#' class(edhec)
#' # Changing the data colnames
#' names(edhec) = c("CA", "CTA", "DIS", "EM", "EMN",
#'                  "ED", "FIA", "GM", "LS", "MA",
#'                  "RV", "SS", "FOF")
#' # Compute Rachev ratio for managers data
#' DownsideSharpeRatio(edhec)
#'
DownsideSharpeRatio <- function(R, rf=0,
                                SE=FALSE, SE.control=NULL,
                                ...){
  
  
  R = checkData(R, method="matrix")
  
  # Adjusting the returns if rf is a vector of same length
  if(length(rf)>1){
    R <- apply(R, 2, function(x, rf) return(x-as.numeric(rf)), rf=rf)
    rf <- 0
  }
  
  # Checking input if SE = TRUE
  if(SE){
    SE.check <- TRUE
    if(!requireNamespace("RPESE", quietly = TRUE)){
      warning("Package \"RPESE\" needed for standard errors computation. Please install it.",
              call. = FALSE)
      SE <- FALSE
    }
  }
  
  # SE Computation
  if(SE){
    
    # Setting the control parameters
    if(is.null(SE.control))
      SE.control <- RPESE.control(estimator="DSR")
    
    # Computation of SE (optional)
    ses=list()
    # For each of the method specified in se.method, compute the standard error
    for(mymethod in SE.control$se.method){
      ses[[mymethod]]=RPESE::EstimatorSE(R, estimator.fun = "DSR", se.method = mymethod, 
                                         cleanOutliers=SE.control$cleanOutliers,
                                         fitting.method=SE.control$fitting.method,
                                         freq.include=SE.control$freq.include,
                                         freq.par=SE.control$freq.par,
                                         a=SE.control$a, b=SE.control$b,
                                         rf=rf, 
                                         ...)
      ses[[mymethod]]=ses[[mymethod]]$se
    }
    ses <- t(data.frame(ses))
  }
  
  # Downside SR
  DSR <- function(returns, rf = 0){
    
    # Computing the mean of the returns
    mu.hat <- mean(returns)
    # Computing the SemiSD
    semisd <- sqrt((1/length(returns))*sum((returns-mu.hat)^2*(returns <= mu.hat)))
    # Computing the SemiMean
    semimean <- (1/length(returns))*sum((returns-mu.hat)*(returns <= mu.hat))
    # Computing DSR of the returns
    DSR <- (mu.hat-rf)/(semisd*sqrt(2))
    
    # Returning estimate
    return(DSR)
  }
  
  # Computation of Rachev Ratio
  myDSR <- t(apply(R, 2, DSR, rf=rf))
  rownames(myDSR) <- "Downside Sharpe Ratio"
  
  if(SE) # Check if SE computation
    return(rbind(myDSR, ses)) else
      return (myDSR)
}

