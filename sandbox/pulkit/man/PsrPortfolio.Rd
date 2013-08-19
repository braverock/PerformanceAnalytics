\name{PsrPortfolio}
\alias{PsrPortfolio}
\title{Implementation of PSR Portfolio Optimization}
\usage{
  PsrPortfolio(R, refSR = 0, bounds = NULL, MaxIter = 1000,
    delta = 0.005)
}
\arguments{
  \item{R}{The return series}

  \item{refSR}{The benchmark Sharpe Ratio}

  \item{bounds}{The bounds for the weights}

  \item{MaxIter}{The Maximum number of iterations}

  \item{delta}{The value of delta Z}
}
\description{
  Maximizing for PSR leads to better diversified and more
  balanced hedge fund allocations compared to the
  concentrated outcomes of Sharpe ratio maximization.We
  would like to find the vector of weights that maximize
  the expression

  \deqn{\hat{PSR}(SR^\**) =
  Z\biggl[\frac{(\hat{SR}-SR^\**)\sqrt{n-1}}{\sqrt{1-\hat{\gamma_3}SR^\**
  + \frac{\hat{\gamma_4}-1}{4}\hat{SR^2}}}\biggr]}

  where \eqn{\sigma = \sqrt{E[(r-\mu)^2]}} ,its standard
  deviation.\eqn{\gamma_3=\frac{E\biggl[(r-\mu)^3\biggr]}{\sigma^3}}
  its skewness,
  \eqn{\gamma_4=\frac{E\biggl[(r-\mu)^4\biggr]}{\sigma^4}}
  its kurtosis and \eqn{SR = \frac{\mu}{\sigma}} its Sharpe
  Ratio. Because \eqn{\hat{PSR}(SR^\**)=Z[\hat{Z^\**}]} is
  a monotonic increasing function of \eqn{\hat{Z^\**}} ,it
  suffices to compute the vector that maximizes
  \eqn{\hat{Z^\**}}

  This optimal vector is invariant of the value adopted by
  the parameter \eqn{SR^\**}. Gradient Ascent Logic is used
  to compute the weights using the Function PsrPortfolio
}
\examples{
data(edhec)
PsrPortfolio(edhec)
}
\author{
  Pulkit Mehrotra
}
\references{
  Bailey, David H. and Lopez de Prado, Marcos, \emph{The
  Sharpe Ratio Efficient Frontier} (July 1, 2012). Journal
  of Risk, Vol. 15, No. 2, Winter 2012/13
}
\seealso{
  \code{\link{ProbSharpeRatio}} \code{\link{table.PSR}}
  \code{\link{MinTrackRecord}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

