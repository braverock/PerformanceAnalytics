#' Martin ratio of the return distribution
#'
#' To calculate Martin ratio we divide the difference of the portfolio return
#' and the risk free rate by the Ulcer index
#'
#' \deqn{Martin ratio = \frac{r_P - r_F}{\sqrt{\sum^{n}_{i=1} \frac{{D'_i}^2}{n}}}}{Martin ratio = (rp - rf) / Ulcer index}
#'
#' where \eqn{r_P} is the annualized portfolio return, \eqn{r_F} is the risk free
#' rate, \eqn{n} is the number of observations of the entire series, \eqn{D'_i} is
#' the drawdown since previous peak in period i
#'
#' @aliases MartinRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.91
#' 
###keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon)
#' print(MartinRatio(portfolio_bacon[,1])) #expected 1.70
#'
#' data(managers)
#' print(MartinRatio(managers['1996']))
#' print(MartinRatio(managers['1996',1])) 
#'
#' @export


MartinRatio <- function (R, Rf = 0, ...) 
{
    R = checkData(R)

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       calcul = FALSE
        for (i in (1:length(R))) {
     	     if (!is.na(R[i])) {
     	    	calcul = TRUE
	     }
        }		      
       period = Frequency(R)
       UI = UlcerIndex(R)
       R = na.omit(R)
       n = length(R)
       if(!calcul) {
	  result = NA
	}
	else {
	   Rp = (prod(1 + R))^(period / length(R)) - 1
       	   result = (Rp - Rf) / UI
	}
       return(result)
    }
    else {
        result = apply(R, MARGIN = 2, MartinRatio, Rf = Rf, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Martin Ratio (Rf = ",Rf,")", sep="")
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
