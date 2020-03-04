# collection of mean-related statistics
#
# NOTE: we would prefer that the first argument be R for returns,
#       but the package check complains about mismatch of the
#       first parameter with R command mean()


#' calculate attributes relative to the mean of the observation series given,
#' including geometric, stderr, LCL and UCL
#' 
#' \tabular{ll}{ \code{\link{mean.geometric}} \tab geometric mean \cr
#' \code{\link{mean.stderr}} \tab standard error of the mean (S.E. mean) \cr
#' \code{\link{mean.LCL}} \tab lower confidence level (LCL) of the mean \cr
#' \code{\link{mean.UCL}} \tab upper confidence level (UCL) of the mean \cr }
#' 
#' 
#' @aliases mean.utils mean.geometric mean.UCL mean.LCL mean.stderr Mean.arithmetic
#' @param x a vector, matrix, data frame, or time series to calculate the
#' modified mean statistic over
#' @param ci the confidence interval to use
#' @param SE TRUE/FALSE whether to ouput the standard errors of the estimates of the risk measures, default FALSE. Only available for  \code{\link{Mean.arithmetic}}.
#' @param SE.control Control parameters for the computation of standard errors. Should be done using the \code{\link{RPESE.control}} function.
#' Only available for \code{\link{Mean.arithmetic}}.
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link[stats]{sd}} \cr \code{\link[base]{mean}}
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' mean.geometric(edhec[,"Funds of Funds"])
#' mean.stderr(edhec[,"Funds of Funds"])
#' mean.UCL(edhec[,"Funds of Funds"])
#' mean.LCL(edhec[,"Funds of Funds"])
#' @rdname mean.geometric
## @method mean geometric
#' @export mean.geometric
mean.geometric <-
function (x, ...)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the mean geometric return for a return series

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns the geometric return

    # FUNCTION:
    if (is.vector(x)) {
        x = na.omit(x)
        mean.geometric = exp(mean(log(1+x)))-1
        return(mean.geometric)
    }
    else {
        x = checkData(x, method = "matrix", ... = ...)
        result = apply(x, 2, mean.geometric, ... = ...)
        dim(result) = c(1,NCOL(x))
        colnames(result) = colnames(x)
        rownames(result) = "Geometric Mean"
        return(result)
    }
}

#' @rdname mean.geometric
#' @method Mean arithmetic
#' @export Mean.arithmetic
Mean.arithmetic <-
  function (x,
            SE=FALSE, SE.control=NULL, ...)
  {# @author Peter Carl
    
    # DESCRIPTION
    # Calculates the mean arithmetic return for a return series
    
    # Inputs:
    # R: Assumes returns rather than prices
    # SE: Boolean to determine if the SE of the estimate(s) is returned
    # se.method: SE method used for the estimate
    
    # Output:
    # Returns the geometric return
    
    # Option to check if RPESE is installed if SE=TRUE
    if(isTRUE(SE)){
      if(!requireNamespace("RPESE", quietly = TRUE)){
        stop("Package \"pkg\" needed for standard errors computation. Please install it.",
             call. = FALSE)
      }
      
      # Setting the control parameters
      if(is.null(SE.control))
        SE.control <- RPESE.control(estimator="Mean")
      
      # Computation of SE (optional)
      ses=list()
      # For each of the method specified in se.method, compute the standard error
      for(mymethod in SE.control$se.method){
        ses[[mymethod]]=RPESE::EstimatorSE(x, estimator.fun = "Mean", se.method = mymethod, 
                                           cleanOutliers=SE.control$cleanOutliers,
                                           fitting.method=SE.control$fitting.method,
                                           freq.include=SE.control$freq.include,
                                           freq.par=SE.control$freq.par,
                                           a=SE.control$a, b=SE.control$b,
                                           ...)
        ses[[mymethod]]=ses[[mymethod]]$se
      }
      ses <- t(data.frame(ses))
    }
    
    # FUNCTION:
    if (is.vector(x)) {
      x = na.omit(x)
      Mean.arithmetic = mean(x)
      
      if(SE) # Check if computation of SE
        return(rbind(Mean.arithmetic, ses)) else
          return(Mean.arithmetic)
    }
    else {
      x = checkData(x, method = "matrix", ... = ...)
      result = apply(x, 2, mean, ... = ...)
      dim(result) = c(1,NCOL(x))
      colnames(result) = colnames(x)
      rownames(result) = "Arithmetic Mean"
      
      if(SE) # Check if computation of SE
        return(rbind(result, ses)) else
          return(result)
    }
  }

#' @rdname mean.geometric
#' @method mean stderr
#' @export mean.stderr
mean.stderr <-
function (x, ...)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the standard error of the mean for a return series

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns the standard error of the mean for the return

    # FUNCTION:
    if (is.vector(x)) {
        x = na.omit(x)
        stderr = sqrt(var(x)/length(x))
        return(stderr)
    }
    else {
        x = checkData(x, method = "matrix", ... = ...)
        result = apply(x, 2, mean.stderr, ... = ...)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(x)
        rownames(result) = "Standard Error"
        return(result)
    }
}

#' @rdname mean.geometric
## @method mean LCL
#' @export mean.LCL
mean.LCL <-
function (x, ci = 0.95, ...)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the standard error of the mean for a return series

    # Inputs:
    # R: Assumes returns rather than prices
    # ci: Confidence interval

    # Output:
    # Uses the standard error of the mean to calculate a lower bound
    # for the confidence interval given

    # FUNCTION:
    if (is.vector(x)) {
        x = na.omit(x)
        n = length(x)
        if (n <= 1)
            return(NA)
        se.mean = sqrt(var(x)/n)
        t.val = qt((1 - ci)/2, n - 1)
        lcl = mean(x) + se.mean * t.val
        return(lcl)
    }
    else {
        x = checkData(x, method = "matrix", ... = ...)
        result = apply(x, 2, mean.LCL, ... = ...)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(x)
        rownames(result) = "Lower Confidence Level"
        return(result)
    }
}

#' @rdname mean.geometric
## @method mean UCL
#' @export mean.UCL
mean.UCL <-
function (x, ci = 0.95, ...)
{# @author Peter Carl

    # DESCRIPTION
    # Calculates the standard error of the mean for a return series

    # Inputs:
    # R: Assumes returns rather than prices
    # ci: Confidence interval

    # Output:
    # Uses the standard error of the mean to calculate an upper bound
    # for the confidence interval given

    # FUNCTION:
    if (is.vector(x)) {
        x = na.omit(x)
        n = length(x)
        if (n <= 1)
            return(NA)
        se.mean = sqrt(var(x)/n)
        t.val = qt((1 - ci)/2, n - 1)
        ucl = mean(x) - se.mean * t.val
        return(ucl)
    }
    else {
        x = checkData(x, method = "matrix", ... = ...)
        result = apply(x, 2, mean.UCL, ... = ...)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(x)
        rownames(result) = "Upper Confidence Level"
        return(result)
    }
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
