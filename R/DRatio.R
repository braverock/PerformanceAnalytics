#' d ratio of the return distribution
#'
#' The d ratio is similar to the Bernado Ledoit ratio but inverted and
#' taking into account the frequency of positive and negative returns.
#'
#' It has values between zero and infinity. It can be used to rank the 
#' performance of portfolios. The lower the d ratio the better the 
#' performance, a value of zero indicating there are no returns less than
#' zero and a value of infinity indicating there are no returns greater than zero.
#'
#' \deqn{DRatio(R) = \frac{n_{d}*\sum^{n}_{t=1}{max(-R_{t},0)}}{n_{u}*\sum^{n}_{t=1}
#' {max(R_{t},0)}}}{DRatio(R) = nd*sum
#' (t=1..n)(max(-R(t),0)) / nu*sum(t=1..n)(max(R(t),0))}
#'
#' where \eqn{n} is the number of observations of the entire series,
#'       \eqn{n_{d}} is the number of observations less than zero,
#'       \eqn{n_{u}} is the number of observations greater than zero
#' 
#' @aliases DRatio
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
#' print(DRatio(portfolio_bacon[,1])) #expected 0.401
#'
#' data(managers)
#' print(DRatio(managers['1996']))
#' print(DRatio(managers['1996',1])) #expected 0.0725
#'
#' @export

DRatio <- function (R, ...)
{
    R = checkData(R)

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       R = na.omit(R)
       r1 = R[which(R > 0)]
       r2 = R[which(R < 0)]
       nd = length(r2)
       nu = length(r1)
       result = (-nd*sum(r2))/(nu*sum(r1))    
       return(result)
    }  
    else {
        result = apply(R, MARGIN = 2, DRatio, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("d ratio", sep="")
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
