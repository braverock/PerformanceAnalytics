\name{Return.calculate}
\alias{CalculateReturns}
\alias{Return.calculate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate simple or compound returns from prices }
\description{
calculate simple or compound returns from prices
}
\usage{
Return.calculate(prices, method=c("compound","simple"))
CalculateReturns(prices, method=c("compound","simple"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prices}{ data object containing ordered price observations }
  \item{method}{ calculate "simple" or "compound" returns, default compound }
}
\value{
vector or matrix of simple or compound returns
}
\details{
Two requirements should be made clear.  First, the function \code{Return.calculate} assumes regular price data.  In this case, we downloaded monthly close prices.  Prices can be for any time scale, such as daily, weekly, monthly or annual, as long as the data consists of regular observations.  Irregular observations require time period scaling to be comparable.  Fortunately, \code{\link[xts]{to.period}} in the \code{xts} package, or the \code{\link[zoo]{aggregate.zoo}} in the \code{zoo} package supports supports management and conversion of irregular time series.

Second, if corporate actions, dividends, or other adjustments such as time- or money-weighting are to be taken into account, those calculations must be made separately. This is a simple function that assumes fully adjusted close prices as input.  For the IBM timeseries in the example below, dividends and corporate actions are not contained in the "close" price series, so we end up with "price returns" instead of "total returns".  This can lead to significant underestimation of the return series over longer time periods.  To use adjusted returns, specify \code{quote="AdjClose"} in \code{\link[tseries]{get.hist.quote}}, which is found in package \code{tseries}.
}
\references{ Bacon, C. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. Chapter 2 \cr
}
\author{ Peter Carl }
\seealso{
    \code{\link{Return.cumulative}}
}
\examples{
require(tseries)
prices = get.hist.quote("IBM", start = "1999-01-01", end = "2007-01-01", quote = "AdjClose", compression = "d")
R.IBM = Return.calculate(prices, method="simple")
R.IBM = as.xts(R.IBM)
colnames(R.IBM)="IBM"
chart.CumReturns(R.IBM,legend.loc="topleft", main="Cumulative Daily Returns for IBM")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
