#' Frequency of the return distribution
#'
#' Gives the period of the return distribution (ie 12 if monthly return, 4 if quarterly return)
#'
#' @aliases Frequency
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' 
###keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon)
#' print(Frequency(portfolio_bacon[,1])) #expected 12
#' data(managers)
#' print(Frequency(managers['1996',1:5]))
#'
#' @export 

Frequency <- function (R, ...)
{
    R = checkData(R)

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {

    calcul = FALSE    
     for (i in (1:length(R))) {
     	 if (!is.na(R[i])) {
     	    calcul = TRUE
	 }
      }

     if (calcul) {
       freq = periodicity(R)
       switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {result = 252},  #252 trading days in any given year
            weekly = {result = 52},
            monthly = {result = 12},
            quarterly = {result = 4},
            yearly = {result = 1}
        )
	}
	else
	{
	 result = NA
	}
       return(result)
    }  
    else {
        result = apply(R, MARGIN = 2, Frequency, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Frequency", sep="")
        return(result)
    }
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
