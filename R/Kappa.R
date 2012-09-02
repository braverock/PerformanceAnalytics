#' Kappa of the return distribution
#'
#' Introduced by Kaplan and Knowles (2004), Kappa is a generalized
#' downside risk-adjusted performance measure.
#'
#' To calculate it, we take the difference of the mean of the distribution
#' to the target and we divide it by the l-root of the lth lower partial
#' moment. To calculate the lth lower partial moment we take the subset of
#' returns below the target and we sum the differences of the target to
#' these returns. We then return return this sum divided by the length of
#' the whole distribution.
#'
#' \deqn{Kappa(R, MAR, l) = \frac{r_{p}-MAR}{\sqrt[l]{\frac{1}{n}*\sum^n_{t=1}
#' max(MAR-R_{t}, 0)^l}}}{Kappa(R, MAR, l) = (rp - MAR)/(\sqrt[l](1/n*sum(t=1..n)
#' (max(MAR-r(t),0)^l)))}
#'
#' For l=1 kappa is the Sharpe-omega ratio and for l=2 kappa
#' is the sortino ratio.
#'
#' Kappa should only be used to rank portfolios as it is difficult to
#' interpret the absolute differences between kappas. The higher the
#' kappa is, the better.
#'
#'
#' @aliases Kappa
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param l the coefficient of the Kappa
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.96
#' 
#' @keywords ts multivariate distribution models
#'
#' @examples
#' l = 2
#'
#' data(portfolio_bacon)
#' MAR = 0.005
#' print(Kappa(portfolio_bacon[,1], MAR, l)) #expected 0.157
#'
#' data(managers)
#' MAR = 0
#' print(Kappa(managers['1996'], MAR, l))
#' print(Kappa(managers['1996',1], MAR, l)) #expected 1.493
#'
#' @export 

Kappa <- function (R, MAR, l, ...)
{
    R = checkData(R)

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       R = na.omit(R)
       r = R[which(R < MAR)]
       n = length(R)
       m = mean(R)
       result = (m-MAR)/(((1/n)*sum((MAR - r)^l))^(1/l))
       return(result)
    }
    else {
        result = apply(R, MARGIN = 2, Kappa, MAR=MAR, l=l, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("kappa (MAR = ",MAR,"%)", sep="")
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
