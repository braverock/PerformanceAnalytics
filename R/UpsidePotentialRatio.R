#' calculate Upside Potential Ratio of upside performance over downside risk
#' 
#' Sortino proposed an improvement on the Sharpe Ratio to better account for
#' skill and excess performance by using only downside semivariance as the
#' measure of risk.  That measure is the \code{\link{SortinoRatio}}. This
#' function, Upside Potential Ratio, was a further improvement, extending the
#' measurement of only upside on the numerator, and only downside of the
#' denominator of the ratio equation.
#' 
#' calculate Upside Potential Ratio of upside performance over downside risk
#' 
#' Sortino proposed an improvement on the Sharpe Ratio to better account for
#' skill and excess performance by using only downside semivariance as the
#' measure of risk.  That measure is the \code{\link{SortinoRatio}}. This
#' function, Upside Potential Ratio, was a further improvement, extending the
#' measurement of only upside on the numerator, and only downside of the
#' denominator of the ratio equation.
#' 
#' 
#' Sortino contends that risk should be measured in terms of not meeting the
#' investment goal.  This gives rise to the notion of \dQuote{Minimum
#' Acceptable Return} or MAR.  All of Sortino's proposed measures include the
#' MAR, and are more sensitive to downside or extreme risks than measures that
#' use volatility(standard deviation of returns) as the measure of risk.
#' 
#' Choosing the MAR carefully is very important, especially when comparing
#' disparate investment choices.  If the MAR is too low, it will not adequately
#' capture the risks that concern the investor, and if the MAR is too high, it
#' will unfavorably portray what may otherwise be a sound investment.  When
#' comparing multiple investments, some papers recommend using the risk free
#' rate as the MAR.  Practitioners may wish to choose one MAR for consistency,
#' several standardized MAR values for reporting a range of scenarios, or a MAR
#' customized to the objective of the investor.
#' 
#' \deqn{ UPR=\frac{ \sum^{n}_{t=1} (R_{t} - MAR) }{ \delta_{MAR} } } where
#' \eqn{\delta_{MAR}} is the \code{\link{DownsideDeviation}}.
#' 
#' The numerator in \code{UpsidePotentialRatio} only uses returns that exceed
#' the MAR, and the denominator (in \code{\link{DownsideDeviation}}) only uses
#' returns that fall short of the MAR by default.  Sortino contends that this
#' is a more accurate and balanced protrayal of return potential, wherase
#' \code{\link{SortinoRatio}} can reward managers most at the peak of a cycle,
#' without adequately penalizing them for past mediocre performance.  Others
#' have used the full series, and this is provided as an option by the
#' \code{method} argument.
#' 
#' @aliases UpsidePotentialRatio UPR
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param method one of "full" or "subset", indicating whether to use the
#' length of the full series or the length of the subset of the series
#' above(below) the MAR as the denominator, defaults to "subset"
#' @author Brian G. Peterson
#' @seealso \code{\link{SharpeRatio}} \cr \code{\link{SortinoRatio}} \cr
#' \code{\link{DownsideDeviation}} \cr \code{\link{SemiVariance}} \cr
#' \code{\link{SemiDeviation}} \cr \code{\link{InformationRatio}}
#' @references Sortino, F. and Price, L. Performance Measurement in a Downside
#' Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65.
#' 
#' Plantinga, A., van der Meer, R. and Sortino, F. The Impact of Downside Risk
#' on Risk-Adjusted Performance of Mutual Funds in the Euronext Markets. July
#' 19, 2001. Available at SSRN: \url{http://ssrn.com/abstract=277352}
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' UpsidePotentialRatio(edhec[, 6], MAR=.05/12) #5 percent/yr MAR
#' UpsidePotentialRatio(edhec[, 1:6], MAR=0)
#' @export
UpsidePotentialRatio <-
function (R, MAR = 0, method=c("subset","full"))
{ # @author Brian G. Peterson

    # Description:
    # Sortino proposed to better account for skill and excess peRformance
    # by using only downside semivariance as the measure of risk.
    # UpsidePotentialRatio is an attempted improvement over the SortinoRatio

    # Ra    return vector
    # MAR   minimum acceptable return

    # Function:

    method = method[1] 

    if (is.vector(R)) {
        if(!is.null(dim(MAR))){
            if(is.timeBased(index(MAR))){
                mar <-MAR[index(r)] #subset to the same dates as the R data
            } else{
                mar = mean(checkData(MAR, method = "vector"))
                # we have to assume that Ra and a vector of Rf passed in for MAR both cover the same time period
            }   
        } else mar=MAR
        r = subset(R, R > mar)
        switch(method,
            full   = {len = length(R)},
            subset = {len = length(r)} #previously length(R)
        ) # end switch
        excess=-mar+r
        result = (sum(excess)/len)/DownsideDeviation(R, MAR=MAR , method=method)
        return(result)
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, MARGIN = 2, UpsidePotentialRatio, MAR = MAR, method = method)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = paste("Upside Potential (MAR = ",round(mean(MAR)*100,1),"%)", sep="")
        return(result)
    }
}

UPR <-
function (R, MAR = 0, method=c("subset","full"))
{ # @author Brian G. Peterson
    UpsidePotentialRatio(R=R, MAR=MAR, method=method)
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
