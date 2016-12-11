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

    LPM = (1/nb) * sum(max(0,R_avg-x_cond)^n)
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

    LPM = (1/nb) * sum(max(0,R_avg-x_cond)^n)
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
    #Loop close
    colnames(result) = columnnames
    #Assign column names to the output 
    rownames(result) = "LPM"
    #Assign rowname to the output
  return(result)
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
