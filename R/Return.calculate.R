#' calculate simple or compound returns from prices
#' 
#' calculate simple or compound returns from prices
#' 
#' Two requirements should be made clear.  First, the function
#' \code{Return.calculate} assumes regular price data.  In this case, we
#' downloaded monthly close prices.  Prices can be for any time scale, such as
#' daily, weekly, monthly or annual, as long as the data consists of regular
#' observations.  Irregular observations require time period scaling to be
#' comparable.  Fortunately, \code{\link[xts]{to.period}} in the \code{xts}
#' package, or the \code{\link[zoo]{aggregate.zoo}} in the \code{zoo} package
#' supports supports management and conversion of irregular time series.
#' 
#' Second, if corporate actions, dividends, or other adjustments such as time-
#' or money-weighting are to be taken into account, those calculations must be
#' made separately. This is a simple function that assumes fully adjusted close
#' prices as input.  For the IBM timeseries in the example below, dividends and
#' corporate actions are not contained in the "close" price series, so we end
#' up with "price returns" instead of "total returns".  This can lead to
#' significant underestimation of the return series over longer time periods.
#' To use adjusted returns, specify \code{quote="AdjClose"} in
#' \code{\link[tseries]{get.hist.quote}}, which is found in package
#' \code{tseries}.
#' 
#' @aliases CalculateReturns Return.calculate
#' @param prices data object containing ordered price observations
#' @param method calculate "simple" or "compound" returns, default compound
#' @author Peter Carl
#' @seealso \code{\link{Return.cumulative}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. Chapter 2 \cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#'   \dontrun{
#'     require(tseries)
#'     prices = get.hist.quote("IBM", start = "1999-01-01", end = "2007-01-01", quote = "AdjClose", compression = "d")
#'   }
#'   \dontshow{
#'     data(prices)
#'   }
#' R.IBM = Return.calculate(prices, method="simple")
#' R.IBM = as.xts(R.IBM)
#' colnames(R.IBM)="IBM"
#' chart.CumReturns(R.IBM,legend.loc="topleft", main="Cumulative Daily Returns for IBM")
#' round(R.IBM,2)
#' 
Return.calculate <-
function(prices, method = c("compound","simple"))
{ # @ author Peter Carl

    #  Calculate returns from a price stream

    # Required inputs

    # Prices: data object containing ordered price observations
    # method: "simple", "compound"

    # FUNCTION:

    method = method[1]
    pr = checkData(prices, method = "xts")

    if(method=="simple")
        Returns = pr/pr[-nrow(pr), ] - 1
        # Returns = pr/lag(pr,k=1) - 1

    if(method=="compound") {
        Returns = diff(log(pr))
    }

    reclass(Returns,match.to=pr)

}

CalculateReturns <-
function(prices, method = c("compound","simple"))
{ # @ author Peter Carl
    Return.calculate(prices=prices, method=method)
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
