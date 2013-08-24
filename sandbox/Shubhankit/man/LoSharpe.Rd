\name{LoSharpe}
\alias{LoSharpe}
\title{Andrew Lo Sharpe Ratio}
\usage{
  LoSharpe(Ra, Rf = 0, q = 3, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of daily asset returns}

  \item{Rf}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of annualized Risk Free Rate}

  \item{q}{Number of autocorrelated lag periods. Taken as 3
  (Default)}

  \item{\dots}{any other pass thru parameters}
}
\description{
  Although the Sharpe ratio has become part of the canon of
  modern financial analysis, its applications typically do
  not account for the fact that it is an estimated
  quantity, subject to estimation errors that can be
  substantial in some cases.

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
  \sum(q-k)\rho(k)] } Where k belongs to 0 to q-1
}
\examples{
data(edhec)
head(LoSharpe(edhec,0,3)
}
\author{
  Brian G. Peterson, Peter Carl, Shubhankit Mohan
}
\references{
  Getmansky, Mila, Lo, Andrew W. and Makarov, Igor,\emph{
  An Econometric Model of Serial Correlation and
  Illiquidity in Hedge Fund Returns} (March 1, 2003). MIT
  Sloan Working Paper No. 4288-03; MIT Laboratory for
  Financial Engineering Working Paper No. LFE-1041A-03;
  EFMA 2003 Helsinki Meetings. \code{\link[stats]{}} \cr
  \url{http://ssrn.com/abstract=384700}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{non-iid}
\keyword{ts}

