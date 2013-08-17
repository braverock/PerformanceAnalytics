\name{rollDrawdown}
\alias{rollDrawdown}
\title{Calculate the Rolling Economic Drawdown}
\usage{
  rollDrawdown(R, Rf, h, geometric = TRUE, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeseries,
  or zoo object of asset return.}

  \item{Rf}{risk free rate can be vector such as government
  security rate of return}

  \item{h}{lookback period}

  \item{geometric}{utilize geometric chaining (TRUE) or
  simple/arithmetic chaining(FALSE) to aggregate returns,
  default is TRUE}

  \item{\dots}{any other variable}
}
\description{
  \code{rollDrawdown} calculates the Rolling Economic
  Drawdown(REDD) for a return series.To calculate the
  rolling economic drawdown cumulative return and rolling
  economic max is calculated for each point. The risk free
  return(rf) and the lookback period(h) is taken as the
  input.

  Rolling Economic Drawdown is given by the equation

  \deqn{REDD(t,h)=1-\frac{W_t}/{REM(t,H)}}

  Here REM stands for Rolling Economic Max and is the code
  \code{\link{rollEconomicMax}}
}
\examples{
rollDrawdown(edhec,0.08,100)
}
\references{
  Yang, Z. George and Zhong, Liang, Optimal Portfolio
  Strategy to Control Maximum Drawdown - The Case of Risk
  Based Dynamic Asset Allocation (February 25, 2012)
}

