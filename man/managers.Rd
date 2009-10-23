\name{managers}
\docType{data}
\alias{managers}
\title{Hypothetical Alternative Asset Manager and Benchmark Data}
\description{
A xts object that contains columns of monthly returns for six hypothetical
asset managers (HAM1 through HAM6), the EDHEC Long-Short Equity hedge
fund index, the S\&P 500 total returns, and total return series for
the US Treasury 10-year bond and 3-month bill. Monthly returns for
all series end in December 2006 and begin at different periods starting
from January 1996.

Note that all the EDHEC indices are available in \code{\link{edhec}}.
}
\usage{managers}
\details{
 Please note that the `managers' data set included with PerformanceAnalytics will be periodically updated with new managers and information.  If you intend to use this data set in automated tests, please be sure to subset your data like \code{managers[1:120,1:6]} to use the first ten years of observations on HAM1-HAM6.
}
\format{CSV conformed into an xts object with monthly observations}
\examples{
data(managers)

#preview the data
head(managers)

#summary period statistics
summary(managers)

#cumulative returns
tail(cumprod(1+managers),1)
}
\keyword{datasets}
\keyword{ ts }