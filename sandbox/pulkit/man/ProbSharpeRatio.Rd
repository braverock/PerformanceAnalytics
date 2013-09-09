\name{ProbSharpeRatio}
\alias{ProbSharpeRatio}
\title{Probabilistic Sharpe Ratio}
\usage{
  ProbSharpeRatio(R = NULL, refSR, Rf = 0, p = 0.95,
    weights = NULL, n = NULL, sr = NULL, sk = NULL,
    kr = NULL, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset return}

  \item{Rf}{the risk free rate of return}

  \item{refSR}{the reference Sharpe Ratio, can be a single
  value or a vector for a multicolumn return series.Should
  be non-annualized , in the same periodicity as the
  returns.}

  \item{p}{the confidence level}

  \item{weights}{the weights for the portfolio}

  \item{sr}{Sharpe Ratio, in the same periodicity as the
  returns(non-annualized). To be given in case the return
  series is not given.}

  \item{sk}{Skewness, in the same periodicity as the
  returns(non-annualized). To be given in case the return
  series is not given.}

  \item{kr}{Kurtosis, in the same periodicity as the
  returns(non-annualized). To be given in case the return
  series is not given.}

  \item{n}{track record length. To be given in case the
  return series is not given.}

  \item{\dots}{any other passthru variable}
}
\description{
  Given a predefined benchmark Sharpe ratio ,the observed
  Sharpe Ratio can be expressed in probabilistic terms
  known as the Probabilistic Sharpe Ratio PSR takes higher
  moments into account and delivers a corrected, atemporal
  measure of performance expressed in terms of probability
  of skill. The reference Sharpe Ratio should be less than
  the Observed Sharpe Ratio.

  \deqn{\hat{PSR}(SR^{*}) =
  Z\bigg[\frac{(\hat{SR}-SR^{*})\sqrt{n-1}}{\sqrt{1-\hat{\gamma_3}SR^{*}
  + \frac{\hat{\gamma_4}-1}{4}\hat{SR^2}}}\bigg]} Here
  \eqn{n} is the track record length or the number of data
  points. It can be daily,weekly or yearly depending on the
  input given \eqn{\hat{\gamma{_3}}} and
  \eqn{\hat{\gamma{_4}}} are the skewness and kurtosis
  respectively.
}
\examples{
data(edhec)
ProbSharpeRatio(edhec[,1],refSR = 0.23)
ProbSharpeRatio(refSR = 1/12^0.5,Rf = 0,p=0.95,sr = 2/12^0.5,sk=-0.72,kr=5.78,n=59)
ProbSharpeRatio(edhec[,1:2],refSR = c(0.28,0.24))

data(managers)
ProbSharpeRatio(managers,0)
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
  \code{\link{PsrPortfolio}} \code{\link{table.PSR}}
  \code{\link{MinTrackRecord}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

