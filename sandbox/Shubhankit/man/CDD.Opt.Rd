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
  functional is defined as the mean of the worst (1 -
  \eqn{\alpha})% drawdowns.
}
\details{
  This section formulates a portfolio optimization problem
  with drawdown risk measure and suggests efficient
  optimization techniques for its solving. Optimal asset
  allocation considers: \enumerate{ \item Generation of
  sample paths for the assets' rates of return. \item
  Uncompounded cumulative portfolio rate of return rather
  than compounded one. } Given a sample path of
  instrument's rates of return (r(1),r(2)...,r(N)), the CDD
  functional, \eqn{\delta[\alpha(w)]}, is computed by the
  following optimization procedure \deqn{\delta[\alpha(w)]
  = min y + [1]/[(1-\alpha)N] \sum [z(k)]} s.t. \deqn{z(k)
  greater than u(k)-y } \deqn{u(k) greater than u(k-1)-
  r(k)} which leads to a single optimal value of y equal to
  \eqn{\epsilon(\alpha)} if \eqn{\pi(\epsilon(\alpha)) >
  \alpha}, and to a closed interval of optimal y with the
  left endpoint of \eqn{\epsilon(\alpha)} if
  \eqn{\pi(\epsilon(\alpha)) = \alpha}
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
  Chekhlov, Alexei, Uryasev, Stanislav P. and Zabarankin,
  Michael, \emph{Drawdown Measure in Portfolio
  Optimization} (June 25, 2003). Available at SSRN:
  \url{http://ssrn.com/abstract=544742} or
  \url{http://dx.doi.org/10.2139/ssrn.544742}
}
\seealso{
  CDrawdown.R
}
\keyword{Conditional}
\keyword{Drawdown}
\keyword{models}

