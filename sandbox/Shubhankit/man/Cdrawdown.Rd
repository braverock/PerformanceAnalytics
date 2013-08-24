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

  The \bold{CDD} is related to Value-at-Risk (VaR) and
  Conditional Value-at-Risk (CVaR) measures studied by
  Rockafellar and Uryasev . By definition, with respect to
  a specified probability level \eqn{\alpha}, the
  \bold{\eqn{\alpha}-VaR} of a portfolio is the lowest
  amount \eqn{\epsilon} , \eqn{\alpha} such that, with
  probability \eqn{\alpha}, the loss will not exceed
  \eqn{\epsilon} , \eqn{\alpha} in a specified time T,
  whereas the \bold{\eqn{\alpha}-CVaR} is the conditional
  expectation of losses above that amount \eqn{\epsilon} .
  Various issues about VaR methodology were discussed by
  Jorion . The CDD is similar to CVaR and can be viewed as
  a modification of the CVaR to the case when the
  loss-function is defined as a drawdown. CDD and CVaR are
  conceptually related percentile-based risk performance
  functionals.
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

  Chekhlov, Alexei, Uryasev, Stanislav P. and Zabarankin,
  Michael, \emph{Drawdown Measure in Portfolio
  Optimization} (June 25, 2003). Available at SSRN:
  \url{http://ssrn.com/abstract=544742} or
  \url{http://dx.doi.org/10.2139/ssrn.544742}
}
\keyword{Conditional}
\keyword{Drawdown}
\keyword{models}

