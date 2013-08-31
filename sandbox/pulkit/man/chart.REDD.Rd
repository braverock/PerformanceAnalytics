\name{chart.REDD}
\alias{chart.REDD}
\title{Time series of Rolling Economic Drawdown}
\usage{
  chart.REDD(R, rf, h, geometric = TRUE, legend.loc = NULL,
    colorset = (1:12), ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeseries,
  or zoo object of asset return.}

  \item{rf}{risk free rate can be vector such as government
  security rate of return}

  \item{h}{lookback period}

  \item{geometric}{utilize geometric chaining (TRUE) or
  simple/arithmetic chaining(FALSE) to aggregate returns,
  default is TRUE.}

  \item{legend.loc}{set the legend.loc, as in
  \code{\link{plot}}}

  \item{colorset}{set the colorset label, as in
  \code{\link{plot}}}

  \item{\dots}{any other variable}
}
\description{
  This function plots the time series of Rolling Economic
  Drawdown. For more details on rolling economic drawdown
  see \code{rollDrawdown}.
}
\examples{
data(edhec)
chart.REDD(edhec,0.08,20)
}
\author{
  Pulkit Mehrotra
}
\references{
  Yang, Z. George and Zhong, Liang, Optimal Portfolio
  Strategy to Control Maximum Drawdown - The Case of Risk
  Based Dynamic Asset Allocation (February 25, 2012)
}
\seealso{
  \code{\link{plot}} \code{\link{EconomicDrawdown}}
  \code{\link{EDDCOPS}} \code{\link{rollDrawdown}}
  \code{\link{REDDCOPS}} \code{\link{rollEconomicMax}}
}

