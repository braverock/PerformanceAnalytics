#' Bernardo and Ledoit ratio of the return distribution
#'
#' To calculate Bernardo and Ledoit ratio we take the sum of the subset of
#' returns that are above 0 and we divide it by the opposite of the sum of
#' the subset of returns that are below 0
#'
#' \deqn{BernardoLedoitRatio(R) = \frac{\frac{1}{n}\sum^{n}_{t=1}{max(R_{t},0)}}{\frac{1}{n}\sum^{n}_{t=1}{max(-R_{t},0)}}}{BernardoLedoitRatio(R) = 1/n*sum(t=1..n)(max(R(t),0)) / 1/n*sum(t=1..n)(max(-R(t),0))}
#'
#' where \eqn{n} is the number of observations of the entire series
#' 
#' @aliases BernardoLedoitRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.95
#' 
###keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon)
#' print(BernardoLedoitRatio(portfolio_bacon[,1])) #expected 1.78
#'
#' data(managers)
#' print(BernardoLedoitRatio(managers['1996']))
#' print(BernardoLedoitRatio(managers['1996',1])) #expected 4.598
#'
#' @export 

BernardoLedoitRatio <- function (R, ...)
{
    R <- checkData(R)
    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       R = na.omit(R)
       r1 = R[which(R > 0)]
       r2 = R[which(R < 0)]
       result = sum(r1)/-sum(r2)
       return(result)
    }  
    else {
        result = apply(R, MARGIN = 2, BernardoLedoitRatio, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Bernardo and Ledoit ratio", sep="")
        return(result)
    }
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
