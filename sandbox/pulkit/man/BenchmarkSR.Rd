\name{BenchmarkSR}
\alias{BenchmarkSR}
\title{Benchmark Sharpe Ratio}
\usage{
  BenchmarkSR(R)
}
\arguments{
  \item{R}{a vector, matrix, data frame,timeseries or zoo
  object of asset returns}
}
\description{
  The benchmark SR is a linear function of the average SR
  of the individual strategies, and a decreasing convex
  function of the number of strategies and the average
  pairwise correlation. The Returns are given as the input
  with the benchmark Sharpe Ratio as the output.

  \deqn{SR_B = \bar{SR}\sqrt{\frac{S}{1+(S-1)\bar{\rho}}}}

  Here \eqn{\bar{SR}} is the average SR of the portfolio
  and \eqn{\bar{\rho}} is the average correlation across
  off-diagonal elements
}
\examples{
data(edhec)
BenchmarkSR(edhec) #expected 0.393797
}
\author{
  Pulkit Mehrotra
}
\references{
  Bailey, David H. and Lopez de Prado, Marcos, The Strategy
  Approval Decision: A Sharpe Ratio Indifference Curve
  Approach (January 2013). Algorithmic Finance, Vol. 2, No.
  1 (2013).
}

