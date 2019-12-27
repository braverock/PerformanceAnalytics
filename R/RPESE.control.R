#' @title Controls Function for the Computation of Standard Errors for Risk and Performance Measures
#'
#' @description \code{RPESE.controls} sets the different control parameters used in 
#' the compuation of standard errors for risk and performance measures.
#'
#' @param measure Risk or performance measure used to set default control parameters. Default is "Mean" measure.
#' @param se.method A character string indicating which method should be used to compute
#' the standard error of the estimated standard deviation. One or a combination of:
#' \code{"IFiid"} (default), \code{"IFcor"} (default), \code{"IFcorPW"}, \code{"IFcorAdapt"},
#' \code{"BOOTiid"} or \code{"BOOTcor"}.
#' @param cleanOutliers Boolean variable to indicate whether the pre-whitenning of the influence functions TS should be done through a robust filter.
#' @param fitting.method Distribution used in the standard errors computation. Should be one of "Exponential" (default) or "Gamma".
#' @param freq.include Frequency domain inclusion criteria. Must be one of "All" (default), "Decimate" or "Truncate."
#' @param freq.par Percentage of the frequency used if \code{"freq.include"} is "Decimate" or "Truncate." Default is 0.5.
#' @param a First adaptive method parameter.
#' @param b Second adaptive method parameter.
#'
#' @return A list of different control parameters for the computation of standard errors.
#'
#' @export
#'
#' @author Anthony-Alexander Christidis, \email{anthony.christidis@stat.ubc.ca}
#'
#' @examples
#' # Case where we want the default parameters for the ES
#' ES.control <- RPESE.control(measure="ES")
#' # Case where we also set additional parameters manually
#' ES.control.2 <- RPESE.control(measure="ES", se.method=c("IFcor", "BOOTiid"),
#'                               cleanOutliers=TRUE, freq.include="Decimate")
#' # These lists can be passed onto the functions (e.g., ES) to control the parameters
#' # for computing and returning standard errors.
#' 
# Function to set the control parameters for standard errors computation
RPESE.control <- function(measure=c("Mean","SD","VaR","ES","SR","SoR",
                                    "ESratio", "VaRratio", "SoR", "LPM", 
                                    "OmegaRatio", "SemiSD", "RachevRatio")[1], 
                                    se.method=NULL, 
                                    cleanOutliers=NULL,
                                    fitting.method=NULL,
                                    freq.include=NULL,
                                    freq.par=NULL,
                                    a=NULL,
                                    b=NULL){
  
  # Available estimator functions
  estimators.available <- c("Mean","SD","VaR","ES","SR","SoR",
                            "ESratio", "VaRratio", "SoR", "LPM", "OmegaRatio", "SemiSD", "RachevRatio")
  # Checking if the specified risk measure is available
  if(!(measure %in% estimators.available))
    stop("The specified estimator function is not available.")
  
  # Setting the SE method (based on risk or performance)
  if(measure %in% c("SD", "SemiSD", "LPM", "ES", "VaR"))
    se.method <- c("IFiid","IFcor","IFcorAdapt","IFcorPW","BOOTiid","BOOTcor")[1:2] else 
      if(measure %in% c("Mean", "VaR", "SR", "SoR", "ESratio", "VaRratio", "OmegaRatio", "RachevRatio"))
        se.method <- c("IFiid","IFcor","IFcorAdapt","IFcorPW","BOOTiid","BOOTcor")[c(1,3)]
  
  # Setting the outlier cleaning option
  if(is.null(cleanOutliers))
    cleanOutliers <- FALSE
  
  # Setting the fitting method option
  if(is.null(fitting.method))
    fitting.method <- c("Exponential", "Gamma")[1]
  
  # Setting the frequency inclusion option
  if(is.null(freq.include))
    freq.include <- c("All", "Decimate", "Truncate")[1]
  
  # Setting the frequency proportion parameter option
  if(is.null(freq.par))
    freq.par <- 0.5
  
  # Setting the first adaptive parameter option
  if(is.null(a))
    a <- 0.3
  
  # Setting the second adaptive parameter option
  if(is.null(b))
    b <- 0.7
  
  # Returning the control parameters for SE computation
  return(list(se.method=se.method, cleanOutliers=cleanOutliers, 
              fitting.method=fitting.method, 
              freq.include=freq.include, freq.par=freq.par,
              a=a, b=b))
}
  
  
  
  
  
  
  
  
  
  
  













