\name{weights}
\docType{data}
\alias{weights}
\title{Selected Portfolio Weights Data}
\description{
An xts object that contains columns of monthly weights for a subset of the EDHEC hedge
fund indexes that demonstrate rebalancing portfolios through time.

Note that all the EDHEC indices are available in \code{\link{edhec}}.
}
\usage{managers}
\details{
 A relatively random weights file used for charting examples.
}
\format{CSV conformed into an xts object with monthly observations}
\examples{
data(weights)

#preview the data
head(weights)

}
\keyword{datasets}
