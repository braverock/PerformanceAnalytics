\name{EconomicDrawdown}
\alias{EconomicDrawdown}
\title{Calculate the Economic Drawdown}
\usage{
  EconomicDrawdown(R, Rf, geometric = TRUE, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeseries,
  or zoo object of asset return.}

  \item{Rf}{risk free rate can be vector such as government
  security rate of return}

  \item{geometric}{utilize geometric chaining (TRUE) or
  simple/arithmetic chaining(FALSE) to aggregate returns,
  default is TRUE}

  \item{\dots}{any other variable}
}
\description{
  \code{EconomicDrawdown} calculates the Economic
  Drawdown(EDD) for a return series.To calculate the
  economic drawdown cumulative return and economic max is
  calculated for each point. The risk free return(rf) is
  taken as the input.

  Economic Drawdown is given by the equation

  \deqn{EDD(t)=1-\frac{W_t}/{EM(t)}}

  Here EM stands for Economic Max and is the code
  \code{\link{EconomicMax}}
}
\examples{
EconomicDrawdown(edhec,0.08,100)
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
  \code{\link{chart.REDD}} \code{\link{EDDCOPS}}
  \code{\link{rollDrawdown}} \code{\link{REDDCOPS}}
  \code{\link{rollEconomicMax}}
}

