#' calculate a lower partial moment for a time series
#' 
#' Caclulate a Lower Partial Moment around the mean or a specified threshold.
#' 
#' Lower partial moments capture negative deviation from a reference point.  
#' That reference point may be the mean, or some specified threshold that
#' has other meaning for the investor.
#' 
#' @references Huffman S.P. & Moll C.R., 
#' "The impact of Asymmetry on Expected Stock Returns: An Investigation of Alternative Risk Measures", 
#' Algorithmic Finance 1, 2011 p. 79-93
#'
#' @param R xts data
#' @param n the n-th moment to return
#' @param threshold threshold can be the mean or any point as desired
#' @param about_mean TRUE/FALSE calculate LPM about the mean under the threshold or use the threshold to calculate the LPM around (if FALSE)
#' @param SE TRUE/FALSE whether to ouput the standard errors of the estimates of the risk measures, default FALSE.
#' @param SE.control Control parameters for the computation of standard errors. Should be done using the \code{\link{RPESE.control}} function.
#' @param ... Additional parameters.
#' 
#' @author Kyle Balkissoon \email{kylebalkisoon@@gmail.com}
#' @export
lpm <- function(R, n=2, threshold=0, about_mean=FALSE,
                SE=FALSE, SE.control=NULL,
                ...){
  
  columns = ncol(R)
  #Calculate number of columns
  columnnames=colnames(R)
  #Get columnnames
  for(column in seq_len(columns)){
  R1 <- checkData(R[,column])
  #Passing single column to R1 from R 
  #Changed variable name from R to R1,not visible to user API
  if(about_mean==TRUE){
    #Calculate Number of obs less than threshold
    nb = nrow(R1[R1<threshold,])
    #Calculate mean of all obs less than threshold
    R_avg = mean(R1[R1<threshold,],na.rm=T)
    #subset data as less than threshold
    x_cond = R1[R1<threshold,]
    #Calculate LPM
    LPM = (1/nb) * sum(pmax(0,R_avg-x_cond)^n)
    LPM=array(LPM)
    #Array of LPM
    if (column==1) {
    #Create data.frame
    result=data.frame(LPM=LPM)
    } else {
    LPM=data.frame(LPM=LPM)
    result[,column]=LPM
    #Inserting the LPM values
    }
  } else {

  #Calculate Number of obs less than threshold
    nb = nrow(R1[R1<threshold,])
    #Calculate mean of all obs less than threshold
    R_avg = threshold
    #subset data as less than threshold
    x_cond = R1[R1<threshold,]
    #Calculate LPM

    LPM = (1/nb) * sum(pmax(0,R_avg-x_cond)^n)
    LPM=array(LPM)
    #Array of LPM
    if (column==1) {
    #Create data.frame
    result=data.frame(LPM=LPM)
    } else {
    LPM=data.frame(LPM=LPM)
    result[,column]=LPM
    #Inserting the LPM values
    }

  }
  }
  
  # Option to check if RPESE is installed if SE=TRUE
  if(isTRUE(SE)){
    if(!requireNamespace("RPESE", quietly = TRUE)){
      stop("Package \"pkg\" needed for standard errors computation. Please install it.",
           call. = FALSE)
    }
    
    # Setting the control parameters
    if(is.null(SE.control))
      SE.control <- RPESE.control(measure="LPM")
    
    # Computation of SE (optional)
    ses=list()
    # For each of the method specified in se.method, compute the standard error
    for(mymethod in SE.control$se.method){
      ses[[mymethod]]=RPESE::EstimatorSE(R, estimator.fun = "LPM", se.method = mymethod, 
                                         cleanOutliers=SE.control$cleanOutliers,
                                         fitting.method=SE.control$fitting.method,
                                         freq.include=SE.control$freq.include,
                                         freq.par=SE.control$freq.par,
                                         a=SE.control$a, b=SE.control$b,
                                         ...)
    }
    ses <- t(data.frame(ses))
  }
  #Loop close
  colnames(result) = columnnames
  #Assign column names to the output 
  rownames(result) = "LPM"
  #Assign rowname to the output
  if(SE) # Check if SE computation
    return(rbind(result, ses)) else
      return(result)
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
