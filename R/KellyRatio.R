#' calculate Kelly criterion ratio (leverage or bet size) for a strategy
#' 
#' Kelly criterion ratio (leverage or bet size) for a strategy.
#' 
#' The Kelly Criterion was identified by Bell Labs scientist John Kelly, and
#' applied to blackjack and stock strategy sizing by Ed Thorpe.
#' 
#' The Kelly ratio can be simply stated as: \dQuote{bet size is the ratio of
#' edge over odds.} Mathematically, you are maximizing log-utility.  As such,
#' the Kelly criterion is equal to the expected excess return of the strategy
#' divided by the expected variance of the excess return, or
#' 
#' \deqn{leverage=\frac{(\overline{R}_{s}-R_{f})}{StdDev(R)^{2}}}{leverage =
#' (mean(R)-Rf=0)/StdDev(R)^2}
#' 
#' As a performance metric, the Kelly Ratio is calculated retrospectively on a
#' particular investment as a measure of the edge that investment has over the
#' risk free rate.  It may be use as a stack ranking method to compare
#' investments in a manner similar to the various ratios related to the Sharpe
#' ratio.
#' 
#' @param R a vector of returns to perform a mean over
#' @param Rf risk free rate, in same period as your returns
#' @param method method=half will use the half-Kelly, this is the default
#' @author Brian G. Peterson
#' @references Thorp, Edward O. (1997; revised 1998). The Kelly Criterion in
#' Blackjack, Sports Betting, and the Stock Market.
#' \url{http://www.bjmath.com/bjmath/thorp/paper.htm} \cr
#' \url{http://en.wikipedia.org/wiki/Kelly_criterion}
#' @keywords ts multivariate distribution models
#' @examples
#' 
#'     data(managers)
#'     KellyRatio(managers[,1,drop=FALSE], Rf=.04/12)
#'     KellyRatio(managers[,1,drop=FALSE], Rf=managers[,10,drop=FALSE])
#'     KellyRatio(managers[,1:6], Rf=managers[,10,drop=FALSE])
#' 
#' @export
KellyRatio <-
function (R, Rf = 0, method = "half")
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # The Kelly Criterion was identified by Bell Labs scientist
    # can be basically stated as
    # bet size is the ratio of edge over odds
    # mathematically, you are maximizing log-utility
    #
    # Kelly criterion says: f should equal the expected excess return of the strategy divided by the expected variance of the excess return, or
    # f = (m-r)/s2

    # FUNCTION:
    R = checkData(R)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    kr <- function (R, Rf, method)
    {
        xR = Return.excess(R, Rf)
        KR =  mean(xR, na.rm=TRUE)/StdDev(R, na.rm=TRUE)^2
        if (method == "half") {
            KR = KR/2
        }
        return(KR)
    }

    result = sapply(R, kr, Rf = Rf, method = method)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Kelly Ratio"
    return (result)
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
