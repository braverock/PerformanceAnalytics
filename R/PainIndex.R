#' Pain index of the return distribution
#'
#' The pain index is the mean value of the drawdowns over the entire 
#' analysis period. The measure is similar to the Ulcer index except that 
#' the drawdowns are not squared.  Also, it's different than the average
#' drawdown, in that the numerator is the total number of observations 
#' rather than the number of drawdowns.
#' 
#' Visually, the pain index is the area of the region that is enclosed by 
#' the horizontal line at zero percent and the drawdown line in the 
#' Drawdown chart.
#'
#' \deqn{Pain index = \sum^{n}_{i=1} \frac{\mid D'_i \mid}{n}}{Pain index = sum(|D'i|/n)}
#'
#' where \eqn{n} is the number of observations of the entire series, \eqn{D'_i} is
#' the drawdown since previous peak in period i
#'
#' @aliases PainIndex
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.89, Becker, Thomas (2006) Zephyr Associates 
#' 
###keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon)
#' print(PainIndex(portfolio_bacon[,1])) #expected 0.04
#'
#' data(managers)
#' print(PainIndex(100*managers['1996']))
#' print(PainIndex(100*managers['1996',1])) 
#'
#' @export 


PainIndex <- function (R, ...) 
{
    
    # DESCRIPTION:
    # 
    # PI = sum[i=1,2,...,n](abs(D'_i)/n) where
    # D'_i = drawdown since previous peak in period i
    # 
    # Becker, Thomas (2006)? Zephyr Associates.

    R = checkData(R)

    pi <- function(R) {
        result = sum(abs(DrawdownPeak(R)))
	R = na.omit(R)
	result = result / length(R)
        return(result)
    }

    result = apply(R, MARGIN = 2, pi)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Pain Index"
    return (result)
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
