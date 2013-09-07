\name{table.Sharpe}
\alias{table.Sharpe}
\title{Sharpe Ratio Statistics Summary}
\usage{
  table.Sharpe(Ra, Rf = 0, q = 3, digits = 4, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of daily asset returns}

  \item{Rf}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of annualized Risk Free Rate}

  \item{q}{Number of autocorrelated lag periods. Taken as 3
  (Default)}

  \item{digits}{Round off Numerical Value}

  \item{\dots}{any other pass thru parameters}
}
\description{
  The Sharpe ratio is simply the return per unit of risk
  (represented by variability).  In the classic case, the
  unit of risk is the standard deviation of the returns.

  \deqn{\frac{\overline{(R_{a}-R_{f})}}{\sqrt{\sigma_{(R_{a}-R_{f})}}}}

  William Sharpe now recommends
  \code{\link{InformationRatio}} preferentially to the
  original Sharpe Ratio.

  The higher the Sharpe ratio, the better the combined
  performance of "risk" and return.

  As noted, the traditional Sharpe Ratio is a risk-adjusted
  measure of return that uses standard deviation to
  represent risk. Although the Sharpe ratio has become part
  of the canon of modern financial analysis, its
  applications typically do not account for the fact that
  it is an estimated quantity, subject to estimation errors
  that can be substantial in some cases.

  Many studies have documented various violations of the
  assumption of IID returns for financial securities.

  Under the assumption of stationarity,a version of the
  Central Limit Theorem can still be applied to the
  estimator .
}
\details{
  The relationship between SR and SR(q) is somewhat more
  involved for non- IID returns because the variance of
  Rt(q) is not just the sum of the variances of component
  returns but also includes all the covariances.
  Specifically, under the assumption that returns \eqn{R_t}
  are stationary, \deqn{ Var[(R_t)] = \sum \sum
  Cov(R(t-i),R(t-j)) = q{\sigma^2} + 2{\sigma^2} \sum
  (q-k)\rho(k) } Where \eqn{ \rho(k) =
  Cov(R(t),R(t-k))/Var[(R_t)]} is the \eqn{k^{th}} order
  autocorrelation coefficient of the series of returns.This
  yields the following relationship between SR and SR(q):
  and i,j belongs to 0 to q-1 \deqn{SR(q) = \eta(q) } Where
  : \deqn{ }{\eta(q) = [q]/[\sqrt(q\sigma^2) + 2\sigma^2
  \sum(q-k)\rho(k)] } Where, k belongs to 0 to q-1 SR(q) :
  Estimated Lo Sharpe Ratio SR : Theoretical William Sharpe
  Ratio
}
\examples{
data(managers)
table.Sharpe(managers,0,3)
}
\author{
  Shubhankit Mohan
}
\references{
  Andrew Lo,\emph{ The Statistics of Sharpe Ratio.}2002,
  AIMR.
  \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=377260}

  Andrew Lo,\emph{Sharpe Ratio may be Overstated}
  \url{http://www.risk.net/risk-magazine/feature/1506463/lo-sharpe-ratios-overstated}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{non-iid}
\keyword{ts}

