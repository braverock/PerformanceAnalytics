#' calculate Sortino Ratio of performance over downside risk
#' 
#' Sortino proposed an improvement on the Sharpe Ratio to better account for
#' skill and excess performance by using only downside semivariance as the
#' measure of risk.
#' 
#' Sortino contends that risk should be measured in terms of not meeting the
#' investment goal.  This gives rise to the notion of \dQuote{Minimum
#' Acceptable Return} or MAR.  All of Sortino's proposed measures include the
#' MAR, and are more sensitive to downside or extreme risks than measures that
#' use volatility(standard deviation of returns) as the measure of risk.
#' 
#' Choosing the MAR carefully is very important, especially when comparing
#' disparate investment choices.  If the MAR is too low, it will not adequately
#' capture the risks that concern the investor, and if the MAR is too high, it
#' will unfavorably portray what may otherwise be a sound investment.  When
#' comparing multiple investments, some papers recommend using the risk free
#' rate as the MAR.  Practitioners may wish to choose one MAR for consistency,
#' several standardized MAR values for reporting a range of scenarios, or a MAR
#' customized to the objective of the investor.
#' 
#' \deqn{ SortinoRatio=\frac{(\overline{R_{a} - MAR})}{\delta_{MAR}} } where
#' \eqn{\delta_{MAR}} is the \code{\link{DownsideDeviation}}.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param \dots any other passthru parameters
#' @param weights portfolio weighting vector, default NULL
#' @param threshold Parameter to determine whether we use a "MAR" (default) or "mean" threshold.
#' @param SE TRUE/FALSE whether to ouput the standard errors of the estimates of the risk measures, default FALSE.
#' @param SE.control Control parameters for the computation of standard errors. Should be done using the \code{\link{RPESE.control}} function.
#' @author Brian G. Peterson
#' @seealso \code{\link{SharpeRatio}} \cr \code{\link{DownsideDeviation}} \cr
#' \code{\link{SemiVariance}} \cr \code{\link{SemiDeviation}} \cr
#' \code{\link{InformationRatio}}
#' @references Sortino, F. and Price, L. Performance Measurement in a Downside
#' Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65.
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' round(SortinoRatio(managers[, 1]),4)
#' round(SortinoRatio(managers[, 1:8]),4)
#' 
#' @export
SortinoRatio <-
function (R, MAR = 0,...,
          weights=NULL,
          threshold = c("MAR", "mean")[1],
          SE=FALSE, SE.control=NULL)
{ # @author Brian G. Peterson
  # modified from function by Sankalp Upadhyay <sankalp.upadhyay [at] gmail [dot] com> with permission

    # Description:
    # Sortino proposed to better account for skill and excess peRformance
    # by using only downside semivariance as the measure of risk.

    # R     return vector
    # MAR   minimum acceptable return
    # Function:
    R = checkData(R)
    
    # Check if mean threshold
    if(threshold=="mean")
      MAR = apply(R, 2, mean)
        
    #if we have a weights vector, use it
    if(!is.null(weights)){
        R=Return.portfolio(R,weights,...)
    }
    
    sr <-function (R, MAR)
    {
        SR = mean(Return.excess(R, MAR), na.rm=TRUE)/DownsideDeviation(R, MAR)
        SR
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
    if(isTRUE(SE)){
      
      # Setting the control parameters
      if(is.null(SE.control))
        SE.control <- RPESE.control(estimator="SoR")
      
      # Setting the threshold parameter
      if(threshold=="MAR")
        threshold.temp <- "const" else if(threshold=="mean")
          threshold.temp <- "mean"
      
      # Computation of SE (optional)
      ses=list()
      # For each of the method specified in se.method, compute the standard error
      for(mymethod in SE.control$se.method){
        ses[[mymethod]]=RPESE::EstimatorSE(R, estimator.fun = "SoR", se.method = mymethod, 
                                           cleanOutliers=SE.control$cleanOutliers,
                                           fitting.method=SE.control$fitting.method,
                                           freq.include=SE.control$freq.include,
                                           freq.par=SE.control$freq.par,
                                           a=SE.control$a, b=SE.control$b,
                                           threshold = threshold.temp, # Parameter for threshold in RPEIF
                                           const=MAR,
                                           ...)
        ses[[mymethod]]=ses[[mymethod]]$se
      }
      ses <- t(data.frame(ses))
    }

    # apply across multi-column data if we have it
    if(threshold=="MAR"){
      result = apply(R, MARGIN = 2, sr, MAR = MAR)
      dim(result) = c(1,NCOL(R))
      colnames(result) = colnames(R)
      rownames(result) = paste("Sortino Ratio (MAR = ", round(mean(MAR)*100,3),"%)", sep="")
    } else if(threshold=="mean"){
      result <- numeric(ncol(R))
      for(col in 1:ncol(R)){
        result[col] <- sr(R[,col], MAR[col])
      }
      result <- matrix(result, ncol=ncol(R))
      colnames(result) = colnames(R)
      rownames(result) <- paste("Sortino Ratio (Mean Threshold)", sep="")
    }
    if(SE) # Check if SE computation
      return(rbind(result, ses)) else
        return (result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2020 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
