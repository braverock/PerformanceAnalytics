\name{CDD.Opt}
\alias{CDD.Opt}
\title{Chekhlov Conditional Drawdown at Risk Optimization}
\usage{
  CDD.Opt(rmat, alpha = 0.05, rmin = 0, wmin = 0, wmax = 1,
    weight.sum = 1)
}
\arguments{
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
}
\details{
  This section formulates a portfolio optimization problem
  with drawdown risk measure and suggests efficient
  optimization techniques for its solving. Optimal asset
  allocation considers: \enumerate{ \item Generation of
  sample paths for the assets' rates of return. \item
  Uncompounded cumulative portfolio rate of return rather
  than compounded one. }
}
\examples{
library(PerformanceAnalytics)
data(edhec)
CDDopt(edhec)
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  DRAWDOWN MEASURE IN PORTFOLIO
  OPTIMIZATION,\emph{International Journal of Theoretical
  and Applied Finance} ,Fall 1994, 49-58.Vol. 8, No. 1
  (2005) 13-58
}
\keyword{Conditional}
\keyword{Drawdown}
\keyword{models}

