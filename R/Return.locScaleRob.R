#' @import RobStatTM
#'
#' @title Robust Filter for Time Series Returns
#'
#' @description \code{Return.locScaleRob} returns the data after passing through a robust location and scale filter.
#'
#' @param R Data of returns for assets or portfolios.
#' @param alpha Tuning parameter for the robust filter.
#' @param normal.efficiency Normal efficiency for robust filter.
#' @param ... Additional parameters.
#'
#' @return A vector of the cleaned data.
#'
#' @export
#'
#' @author Xin Chen, \email{chenx26@uw.edu}
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
#' # Cleaning the returns time series for manager data
#' outRob <- robust.cleaning(edhec$CA)
#'
# Implementation of the robust filter for the IF TS
Return.locScaleRob <- function(R, alpha.robust=0.05, normal.efficiency=0.99){
  
  if(!requireNamespace("RobStatTM", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # Store column names
  column.names.R <- colnames(R)

  # Length of the IF TS
  n <- length(R)
  # Casting as a numerci
  R.numeric <- as.numeric(R)
  # Location and scale parameters
  mu <- RobStatTM::locScaleM(R.numeric)$mu
  s <- RobStatTM::locScaleM(R.numeric)$disper
  
  # Efficiency parameter
  if(normal.efficiency==0.95)
    efficiency.param <- 3 else if(normal.efficiency==0.99)
      efficiency.param <- 3.568 else if(normal.efficiency==0.999)
        efficiency.param <- 4.21 else
          warning("Invalid value for the normal distribution efficiency.")
  
  # Computing the limits
  uplim <- rep(mu + s*efficiency.param, n) 
  dnlim <- rep(mu - s*efficiency.param, n)
  
  # Computing the filtered time series
  xcl <- ifelse(R.numeric >= uplim, uplim, R.numeric)
  xcl <- ifelse(R.numeric <= dnlim, dnlim, xcl)
  
  # Names of original data
  xcl <- matrix(xcl, ncol=1)
  colnames(xcl) <- column.names.R

  # Return the clean data
  return(xcl)
  
}