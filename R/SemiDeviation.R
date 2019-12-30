#' @rdname DownsideDeviation
#' @aliases SemiSD
#' @export
SemiDeviation <- 
function (R,
          SE=FALSE, SE.control=NULL,
          ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function is just a wrapper of DownsideDeviation with
    # MAR = mean(x)
    # see below

    # FUNCTION:
  
  if(isTRUE(SE)){
    if(!requireNamespace("RPESE", quietly = TRUE)){
      stop("Package \"pkg\" needed for standard errors computation. Please install it.",
           call. = FALSE)
    }
    
    # Setting the control parameters
    if(is.null(SE.control))
      SE.control <- RPESE.control(measure="SemiSD")
    
    # Computation of SE (optional)
    ses=list()
    # For each of the method specified in se.method, compute the standard error
    for(mymethod in SE.control$se.method){
      ses[[mymethod]]=RPESE::EstimatorSE(R, estimator.fun = "SemiSD", se.method = mymethod, 
                                         cleanOutliers=SE.control$cleanOutliers,
                                         fitting.method=SE.control$fitting.method,
                                         freq.include=SE.control$freq.include,
                                         freq.par=SE.control$freq.par,
                                         a=SE.control$a, b=SE.control$b,
                                         ...)
    }
    ses <- t(data.frame(ses))
  }

    if (is.vector(R)) {
        R = na.omit(R)
        return(DownsideDeviation(R, MAR=mean(R), method="full"))
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, 2, SemiDeviation)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(R)
        if(SE) # Name if SE computation
          rownames(result) <- "Semi-Standard Deviation" else
            rownames(result) = "Semi-Deviation"
        if(SE) # Check if SE computation
          return(rbind(result, ses)) else
            return (result)
    }
}

#' @rdname DownsideDeviation
#' @export
SemiSD <-
  function (R,
            SE=FALSE, SE.control=NULL,
            ...)
  { # @author Peter Carl
    
    # DESCRIPTION:
    # This function is just a wrapper of DownsideDeviation with
    # MAR = mean(x)
    # see below
    
    # FUNCTION:
    
    if(isTRUE(SE)){
      if(!requireNamespace("RPESE", quietly = TRUE)){
        stop("Package \"pkg\" needed for standard errors computation. Please install it.",
             call. = FALSE)
      }
      
      # Setting the control parameters
      if(is.null(SE.control))
        SE.control <- RPESE.control(measure="SemiSD")
      
      # Computation of SE (optional)
      ses=list()
      # For each of the method specified in se.method, compute the standard error
      for(mymethod in SE.control$se.method){
        ses[[mymethod]]=RPESE::EstimatorSE(R, estimator.fun = "SemiSD", se.method = mymethod, 
                                           cleanOutliers=SE.control$cleanOutliers,
                                           fitting.method=SE.control$fitting.method,
                                           freq.include=SE.control$freq.include,
                                           freq.par=SE.control$freq.par,
                                           a=SE.control$a, b=SE.control$b,
                                           ...)
      }
      ses <- t(data.frame(ses))
    }
    
    if (is.vector(R)) {
      R = na.omit(R)
      return(DownsideDeviation(R, MAR=mean(R), method="full"))
    }
    else {
      R = checkData(R, method = "matrix")
      result = apply(R, 2, SemiDeviation)
      result = matrix(result, nrow=1)
      colnames(result) = colnames(R)
      if(SE) # Name if SE computation
        rownames(result) <- "Semi-Standard Deviation" else
          rownames(result) = "Semi-Deviation"
      if(SE) # Check if SE computation
        return(rbind(result, ses)) else
          return (result)
    }
  }

#' @rdname DownsideDeviation
#' @export
SemiVariance <-
function (R)
{
    if (is.vector(R)) {
        R = na.omit(R)
        return(DownsideDeviation(R, MAR=mean(R), method="subset"))
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, 2, SemiVariance)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = "Semi-Variance"
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2018 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################