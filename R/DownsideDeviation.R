#' downside risk (deviation, variance) of the return distribution
#' 
#' Downside deviation, semideviation, and semivariance are measures of downside
#' risk.
#' 
#' Downside deviation, similar to semi deviation, eliminates positive returns
#' when calculating risk.  Instead of using the mean return or zero, it uses
#' the Minimum Acceptable Return as proposed by Sharpe (which may be the mean
#' historical return or zero). It measures the variability of underperformance
#' below a minimum targer rate. The downside variance is the square of the downside
#' potential.
#' 
#' To calculate it, we take the subset of returns that are less than the target
#' (or Minimum Acceptable Returns (MAR)) returns and take the differences of
#' those to the target.  We sum the squares and divide by the total number of
#' returns to get a below-target semi-variance.
#' 
#'
#' \deqn{DownsideDeviation(R , MAR) = \delta_{MAR} = \sqrt{\sum^{n}_{t=1}\frac{min[(R_{t} - MAR), 0]^2}{n}}}{DownsideDeviation(R, MAR) = sqrt(1/n * sum(t=1..n)((min(R(t)-MAR, 0))^2))}
#'
#' \deqn{ DownsideVariance(R, MAR) = \sum^{n}_{t=1}\frac{min[(R_{t} - MAR), 0]^2}{n}}{DownsideVariance(R, MAR) = 1/n * sum(t=1..n)((min(R(t)-MAR, 0))^2)}
#'
#' \deqn{DownsidePotential(R, MAR) = \sum^{n}_{t=1}\frac{min[(R_{t} - MAR), 0]} {n}}{DownsidePotential(R, MAR) =  1/n * sum(t=1..n)(min(R(t)-MAR, 0))}
#'
#' where \eqn{n} is either the number of observations of the entire series or
#' the number of observations in the subset of the series falling below the
#' MAR.
#' 
#' SemiDeviation or SemiVariance is a popular alternative downside risk measure
#' that may be used in place of standard deviation or variance. SemiDeviation
#' and SemiVariance are implemented as a wrapper of DownsideDeviation with
#' MAR=mean(R).
#' 
#' In many functions like Markowitz optimization, semideviation may be
#' substituted directly, and the covariance matrix may be constructed from
#' semideviation or the vector of returns below the mean rather than from
#' variance or the full vector of returns.
#' 
#' In semideviation, by convention, the value of \eqn{n} is set to the full
#' number of observations. In semivariance the the value of \eqn{n} is set to
#' the subset of returns below the mean.  It should be noted that while this is
#' the correct mathematical definition of semivariance, this result doesn't
#' make any sense if you are also going to be using the time series of returns
#' below the mean or below a MAR to construct a semi-covariance matrix for
#' portfolio optimization.
#' 
#' Sortino recommends calculating downside deviation utilizing a continuous
#' fitted distribution rather than the discrete distribution of observations.
#' This would have significant utility, especially in cases of a small number
#' of observations. He recommends using a lognormal distribution, or a fitted
#' distribution based on a relevant style index, to construct the returns below
#' the MAR to increase the confidence in the final result.  Hopefully, in the
#' future, we'll add a fitted option to this function, and would be happy to
#' accept a contribution of this nature.
#' 
#' @aliases DownsideDeviation SemiDeviation SemiVariance DownsidePotential
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param method one of "full" or "subset", indicating whether to use the
#' length of the full series or the length of the subset of the series below
#' the MAR as the denominator, defaults to "full"
#' @param \dots any other passthru parameters
#' @param potential if TRUE, calculate downside potential instead, default
#' FALSE
#' @author Peter Carl, Brian G. Peterson, Matthieu Lestel
#' @references Sortino, F. and Price, L. Performance Measurement in a Downside
#' Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65. \cr 
#' Carl Bacon, \emph{Practical portfolio performance measurement and attribution}, 
#' second edition 2008
#' 
#' Plantinga, A., van der Meer, R. and Sortino, F. The Impact of Downside Risk
#' on Risk-Adjusted Performance of Mutual Funds in the Euronext Markets. July
#' 19, 2001. Available at SSRN: \url{http://ssrn.com/abstract=277352} \cr
#' 
#' \url{http://www.sortino.com/htm/performance.htm} see especially end note 10
#' 
#' \url{http://en.wikipedia.org/wiki/Semivariance}
#' @keywords ts multivariate distribution models
#' @examples
#'
#' #with data used in Bacon 2008
#'
#' data(portfolio_bacon)
#' MAR = 0.005
#' DownsideDeviation(portfolio_bacon[,1], MAR) #expected 0.0255
#' DownsidePotential(portfolio_bacon[,1], MAR) #expected 0.0137
#' 
#' #with data of managers
#'
#' data(managers)
#' apply(managers[,1:6], 2, sd, na.rm=TRUE)
#' DownsideDeviation(managers[,1:6])  # MAR 0%
#' DownsideDeviation(managers[,1:6], MAR = .04/12) #MAR 4%
#' SemiDeviation(managers[,1,drop=FALSE])
#' SemiDeviation(managers[,1:6])
#' SemiVariance (managers[,1,drop=FALSE])
#' SemiVariance (managers[,1:6]) #calculated using method="subset"
#'
#' @export 
DownsideDeviation <-
function (R, MAR = 0, method=c("full","subset"), ..., potential=FALSE)
{ # @author Peter Carl, Matthieu Lestel

    # DESCRIPTION:
    # Downside deviation, similar to semi deviation, eliminates positive returns
    # when calculating risk.  To calculate it, we take the returns that are less
    # than the target (or Minimum Acceptable Returns (MAR)) returns and take the
    # differences of those to the target.  We sum the squares and divide by the
    # total number of returns to get a below-target semi-variance.

    # This is also useful for calculating semi-deviation by setting
    # MAR = mean(x)

    method = method[1] 
    R = checkData(R, method="matrix")

    if (ncol(R)==1 || is.vector(R) || is.null(R)) {
        R = na.omit(R)

        r = subset(R, R < MAR)

        if(!is.null(dim(MAR))){
            if(is.timeBased(index(MAR))){
                MAR <-MAR[index(r)] #subset to the same dates as the R data
            } else{
                MAR = mean(checkData(MAR, method = "vector"))
                # we have to assume that Ra and a vector of Rf passed in for MAR both cover the same time period
            }   
        }
        
        switch(method,
            full   = {len = length(R)},
            subset = {len = length(r)} #previously length(R)
        ) # end switch

        if(potential) { # calculates downside potential instead
        	 result = sum((MAR - r)/len)
	}
	else {
	     result = sqrt(sum((MAR - r)^2/len))
	}
        return(result)
    }
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, DownsideDeviation, MAR = MAR, method = method)
        result<-t(result)
        colnames(result) = colnames(R)
        if(potential)
            rownames(result) = paste("Downside Potential (MAR = ", round(mean(MAR),1),"%)", sep="")
        else
            rownames(result) = paste("Downside Deviation (MAR = ", round(mean(MAR),1),"%)", sep="")
        return(result)
    }
}

DownsidePotential <-
function (R, MAR=0)
{ # @author Peter Carl, Matthieu Lestel

    # DESCRIPTION:
    # To calculate Downside Potential, we take the returns that are less
    # than the target (or Minimum Acceptable Returns (MAR)) returns and take the
    # differences of those to the target.  We sum and divide by the
    # total number of returns.

    # FUNCTION:
    
    
    if (is.vector(R) || is.null(R) || ncol(R)==1) {
        return(DownsideDeviation(R, MAR=MAR, method="full", potential=TRUE))
    }
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, DownsidePotential, MAR = MAR)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(R)
        rownames(result) = paste("Downside Potential (MAR = ", round(mean(MAR),1),"%)", sep="")
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
