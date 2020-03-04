#' @title Standard Error Estimate for Rachev Ratio of Returns
#'
#' @description \code{RachevRatio.SE} computes the standard error of the Rachev ratio of the returns.
#'
#' @param R Data of returns for one or multiple assets or portfolios.
#' @param alpha Lower tail probability.
#' @param beta Upper tail probability.
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
#' @examples
#' # Loading data from PerformanceAnalytics
#' data(edhec, package = "PerformanceAnalytics")
#' class(edhec)
#' # Changing the data colnames
#' names(edhec) = c("CA", "CTA", "DIS", "EM", "EMN",
#'                  "ED", "FIA", "GM", "LS", "MA",
#'                  "RV", "SS", "FOF")
#' # Compute Rachev ratio for managers data
#' RachevRatio(edhec)
#'
RachevRatio <- function(R, alpha=0.1, beta=0.1, rf=0,
                        SE=FALSE, SE.control=NULL,
                        ...){
  
  
  R = checkData(R, method="matrix")
  
  if(isTRUE(SE)){
    if(!requireNamespace("RPESE", quietly = TRUE)){
      stop("Package \"pkg\" needed for standard errors computation. Please install it.",
           call. = FALSE)
    }
    
    # Setting the control parameters
    if(is.null(SE.control))
      SE.control <- RPESE.control(estimator="RachevRatio")
    
    # Computation of SE (optional)
    ses=list()
    # For each of the method specified in se.method, compute the standard error
    for(mymethod in SE.control$se.method){
      ses[[mymethod]]=RPESE::EstimatorSE(R, estimator.fun = "RachevRatio", se.method = mymethod, 
                                         cleanOutliers=SE.control$cleanOutliers,
                                         fitting.method=SE.control$fitting.method,
                                         freq.include=SE.control$freq.include,
                                         freq.par=SE.control$freq.par,
                                         a=SE.control$a, b=SE.control$b,
                                         alpha=alpha, beta=beta, rf=rf, # Additional Parameters
                                         ...)
      ses[[mymethod]]=ses[[mymethod]]$se
    }
    ses <- t(data.frame(ses))
  }
  
  # Function to compute Rachev Ratio
  RachevRatioFN = function(data, alpha=0.1, beta=0.1, rf=0){
    
    # Computing the mean of the data
    mu.hat <- mean(data)
    # Computing the SD of the data
    sigma.hat <- mean((data-mu.hat)^2)
    
    # Computing the VaR of the data (lower tail)
    VaR.hat.lower <- -quantile(data, alpha)
    # Storing the negative value of the VaR based on the desired alpha (lower tail)
    quantile.lower <- -VaR.hat.lower
    # Computing the ES of the data (lower tail)
    ES.lower <- -mean(data[data<=-VaR.hat.lower])
    
    # Computing the VaR of the data (upper tail)
    n.upper <- floor((1-beta)*length(data))
    sorted.returns <- sort(as.numeric(data))
    VaR.hat.upper <- sorted.returns[n.upper]
    # Storing the negative value of the VaR based on the desired alpha (upper tail)
    quantile.upper <- VaR.hat.upper
    # Computing the ES of the data (upper tail)
    ES.upper <- mean(data[data>=quantile.upper])
    
    # Computing Rachev Ratio
    myRachevRatio <- ES.upper/ES.lower
    
    # Return Rachev Ratio
    return(myRachevRatio)
  }
  
  # Computation of Rachev Ratio
  myRachevRatio = t(apply(R, 2, RachevRatioFN, alpha=0.1, beta=0.1, rf=0))
  rownames(myRachevRatio) = "RachevRatio"
  
  if(SE) # Check if SE computation
    return(rbind(myRachevRatio, ses)) else
      return (myRachevRatio)
}

