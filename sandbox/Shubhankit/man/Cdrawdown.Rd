\name{CDDOpt}
\alias{CDDOpt}
\alias{CDrawdown}
\title{Chekhlov Conditional Drawdown at Risk}
\usage{
  CDDOpt(rmat, alpha = 0.05, rmin = 0, wmin = 0, wmax = 1,
    weight.sum = 1)

  CDrawdown(R, p = 0.9, ...)
}
\arguments{
  \item{Ra}{return vector of the portfolio}

  \item{p}{confidence interval}

  \item{Ra}{return vector of the portfolio}

  \item{p}{confidence interval}
}
\description{
  A new one-parameter family of risk measures called
  Conditional Drawdown (CDD) has been proposed. These
  measures of risk are functionals of the portfolio
  drawdown (underwater) curve considered in active
  portfolio management. For some value of the tolerance
  parameter, in the case of a single sample path, drawdown
  functional is defineed as the mean of the worst (1 -
  \eqn{\alpha})% drawdowns.

  A new one-parameter family of risk measures called
  Conditional Drawdown (CDD) has been proposed. These
  measures of risk are functionals of the portfolio
  drawdown (underwater) curve considered in active
  portfolio management. For some value of the tolerance
  parameter, in the case of a single sample path, drawdown
  functional is defineed as the mean of the worst (1 -
  \eqn{\alpha})% drawdowns.
}
\details{
  This section formulates a portfolio optimization problem
  with drawdown risk measure and suggests e???cient
  optimization techniques for its solving. Optimal asset
  allocation considers: 1) Generation of sample paths for
  the assets' rates of return. 2) Uncompounded cumulative
  portfolio rate of return rather than compounded one.

  The CDD measure generalizes the notion of the drawdown
  functional to a multi-scenario case and can be considered
  as a generalization of deviation measure to a dynamic
  case. The CDD measure includes the Maximal Drawdown and
  Average Drawdown as its limiting cases. The model is
  focused on concept of drawdown measure which is in
  possession of all properties of a deviation
  measure,generalization of deviation measures to a dynamic
  case.Concept of risk profiling - Mixed Conditional
  Drawdown (generalization of CDD).Optimization techniques
  for CDD computation - reduction to linear programming
  (LP) problem. Portfolio optimization with constraint on
  Mixed CDD The model develops concept of drawdown measure
  by generalizing the notion of the CDD to the case of
  several sample paths for portfolio uncompounded rate of
  return.
}
\examples{
library(PerformanceAnalytics)
data(edhec)
CDDopt(edhec)
library(PerformanceAnalytics)
data(edhec)
CDrawdown(edhec)
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan

  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  DRAWDOWN MEASURE IN PORTFOLIO
  OPTIMIZATION,\emph{International Journal of Theoretical
  and Applied Finance} ,Fall 1994, 49-58.Vol. 8, No. 1
  (2005) 13-58

  DRAWDOWN MEASURE IN PORTFOLIO
  OPTIMIZATION,\emph{International Journal of Theoretical
  and Applied Finance} ,Fall 1994, 49-58.Vol. 8, No. 1
  (2005) 13-58
}
\keyword{Conditional}
\keyword{Drawdown}
\keyword{models}

