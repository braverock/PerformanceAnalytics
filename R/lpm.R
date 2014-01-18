#' calculate the lower partial moment of a time series
#'
#' Code to calculate the Lower Partion Moments around the mean or a specified threshold
#' from Huffman S,P & Moll C.R. 2011 "The impact of Asymmetry on Expected Stock Returns: An Investigation of Alternative Risk Measures" Algorithmic Finance 1 (2011) 79-93
#'
#' @param R xts data
#' @param n the n-th moment to return
#' @param threshold threshold can be the mean or any point as desired
#' @param about_mean TRUE/FALSE calculate LPM about the mean under the threshold or use the threshold to calculate the LPM around (if FALSE)
#'
#' @author Kyle Balkissoon /email{kylebalkissoon@gmail.com} /email{kyle@corporateknights.com}
#' @export
lpm <- function(R,n,threshold,about_mean=FALSE){

  if(about_mean==TRUE){
    #Calculate Number of obs less than threshold
    nb = nrow(x[x<threshold,])
    #Calculate mean of all obs less than threshold
    R_avg = mean(x[x<threshold,],na.rm=T)
    #subset data as less than threshold
    x_cond = x[x<threshold,]
    #Calculate LPM

    LPM = (1/nb) * sum(max(0,R_avg-x_cond)^n)
  } else {

  #Calculate Number of obs less than threshold
    nb = nrow(x[x<threshold,])
    #Calculate mean of all obs less than threshold
    R_avg = threshold
    #subset data as less than threshold
    x_cond = x[x<threshold,]
    #Calculate LPM

    LPM = (1/nb) * sum(max(0,R_avg-x_cond)^n)

  }

  return(LPM)
}



###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
