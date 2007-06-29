\name{managers}
\docType{data}
\alias{managers}
\title{Hypothetical Alternative Asset Manager Data and Fixed Income Benchmarks}
\description{
A data frame that contains columns of monthly returns for six hypothetical
asset managers (HAM1 through HAM6), the EDHEC Long-Short Equity hedge
fund index, the S\&P 500 total returns, and total return series for
the US Treasury 10-year bond and 3-month bill. Monthly returns for
all series end in December 2006 and begin at different periods starting
from January 1996.

Note that all the EDHEC indices are available in \code{\link{edhec}}.
}
\usage{managers}
\format{CSV conformed into a data.frame with monthly observations}
\source{}
\references{}
\examples{
data(managers)

#preview the data
head(managers)

#summary period statistics
summary(managers)

#cumulative returns
tail(cumprod.column(1+managers),1)
}
\keyword{datasets}
\keyword{ ts }