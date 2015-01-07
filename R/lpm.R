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
#'
#' @author Kyle Balkissoon \email{kylebalkisoon@@gmail.com}
#' @export
lpm <- function(R,n=2,threshold=0,about_mean=FALSE){

  R <- checkData(R)
  if(about_mean==TRUE){
    #Calculate Number of obs less than threshold
    nb = nrow(R[R<threshold,])
    #Calculate mean of all obs less than threshold
    R_avg = mean(R[R<threshold,],na.rm=T)
    #subset data as less than threshold
    x_cond = R[R<threshold,]
    #Calculate LPM

    LPM = (1/nb) * sum(max(0,R_avg-x_cond)^n)
  } else {

  #Calculate Number of obs less than threshold
    nb = nrow(R[R<threshold,])
    #Calculate mean of all obs less than threshold
    R_avg = threshold
    #subset data as less than threshold
    x_cond = R[R<threshold,]
    #Calculate LPM

    LPM = (1/nb) * sum(max(0,R_avg-x_cond)^n)

  }
  return(LPM)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
