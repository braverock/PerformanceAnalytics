#' Skewness-Kurtosis ratio of the return distribution
#'
#' Skewness-Kurtosis ratio is the division of Skewness by Kurtosis.
#' 
#' It is used in conjunction with the Sharpe ratio to rank portfolios.
#' The higher the rate the better.
#'
#' \deqn{ SkewnessKurtosisRatio(R , MAR) = \frac{S}{K}}{SkewnessKurtosisRatio(R, MAR) = S/K}
#'
#' where \eqn{S} is the skewness and \eqn{K} is the Kurtosis
#'
#' @aliases Skewness-KurtosisRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.100
#' 
###keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(SkewnessKurtosisRatio(portfolio_bacon[,1])) #expected -0.034
#'
#' data(managers)
#' print(SkewnessKurtosisRatio(managers['1996']))
#' print(SkewnessKurtosisRatio(managers['1996',1]))
#'
#' @export 
SkewnessKurtosisRatio <-
function (R, ...)
{
    R = checkData(R)

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       calcul = FALSE
        for (i in (1:length(R))) {
     	     if (!is.na(R[i])) {
     	    	calcul = TRUE
	     }
        }		      
       R = na.omit(R)
        if(!calcul) {
	  result = NA
	}
	else {
	     result = skewness(R, method = "moment") / kurtosis(R, method = "moment")
	}
        return(result)
    }
    else {
        result = apply(R, MARGIN = 2, SkewnessKurtosisRatio, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("SkewnessKurtosisRatio", sep="")
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
