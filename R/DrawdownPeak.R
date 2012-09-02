#' Drawdawn peak of the return distribution
#'
#' Drawdawn peak is for each return its drawdown since the previous peak
#'
#' @aliases DrawdownPeak
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' 
#' @keywords ts multivariate distribution models
#' @export 

DrawdownPeak <- function (R, ...)
{
   R0 <- R
   R = checkData(R, method="matrix")

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
	calcul = FALSE
        for (i in (1:length(R))) {
     	     if (!is.na(R[i])) {
     	    	calcul = TRUE
	     }
        }		      
        if (!calcul) {
	result = NaN
	}
	else {
        R = na.omit(R)
	drawdownpeak = c()
	length(drawdownpeak) = length(R)
	peak = 0
	for(i in (1:length(R))) {
	      val = 1
	      borne = peak+1
	      for(j in (borne:i)) {
	      	    val = val*(1+R[j]/100)
	      }
	      if (val > 1) {
	      	 peak = i
		 drawdownpeak[i] = 0
	      }
	      else {
	      	   drawdownpeak[i] = (val-1)*100
	      }
	}
	result = drawdownpeak
	}
       	reclass(result, R0)	
	return(result)
}
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, DrawdownPeak, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("DrawdownPeak", sep="")
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
