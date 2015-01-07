#' Volatility and variability of the return distribution
#'
#' Volatility skewness is a similar measure to omega but using the second
#' partial moment. It's the ratio of the upside variance compared to the
#' downside variance. Variability skewness is the ratio of the upside risk
#' compared to the downside risk.
#'
#' \deqn{ VolatilitySkewness(R , MAR) = \frac{\sigma_U^2}{\sigma_D^2}}{VolatilitySkewness(R, MAR) = UpsideVariance / DownsideVariance}
#'
#' \deqn{ VariabilitySkewness(R , MAR) = \frac{\sigma_U}{\sigma_D}}{VariabilitySkewness(R, MAR) = UpsideRisk / DownsideRisk}
#'
#' where \eqn{\sigma_U} is the Upside risk and \eqn{\sigma_D} is the Downside Risk
#'
#' @aliases VolatilitySkewness
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param stat one of "volatility", "variability" indicating whether
#' to return the volatility skewness or the variability skweness
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.97-98
#' 
###keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' MAR = 0.005
#' print(VolatilitySkewness(portfolio_bacon[,1], MAR, stat="volatility")) #expected 1.32
#' print(VolatilitySkewness(portfolio_bacon[,1], MAR, stat="variability")) #expected 1.15
#'
#' MAR = 0
#' data(managers)
#'# print(VolatilitySkewness(managers['1996'], MAR, stat="volatility"))
#' print(VolatilitySkewness(managers['1996',1], MAR, stat="volatility"))
#'
#' @export 

VolatilitySkewness <-
function (R, MAR = 0, stat=c("volatility", "variability"), ...)
{
    stat = stat[1]

    R = checkData(R)

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       calcul = FALSE
        for (i in (1:length(R))) {
     	     if (!is.na(R[i])) {
     	    	calcul = TRUE
	     }
        }		      
       R = na.omit(R)

        if(!is.null(dim(MAR))){
            if(is.timeBased(index(MAR))){
                MAR <-MAR[index(R)] 
            } 
	    else{
                MAR = mean(checkData(MAR, method = "vector"))
                # we have to assume that Ra and a vector of Rf passed in for MAR both cover the same time period
            }
        }
        if(!calcul) {
	  result = NA
	}
	else {
	     switch(stat,
		volatility = {result = UpsideRisk(R, MAR, stat="variance")/DownsideDeviation(R,MAR)^2},
	    	variability = {result = UpsideRisk(R, MAR, stat="risk")/DownsideDeviation(R,MAR)},
	    	)
	}
        return(result)
    }
    else {
        result = apply(R, MARGIN = 2, VolatilitySkewness, MAR = MAR, stat = stat, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("VolatilitySkewness (MAR = ",MAR,"%, stat= ",stat,")", sep="")
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
