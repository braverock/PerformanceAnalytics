\name{MinTrackRecord}
\alias{MinTrackRecord}
\title{Minimum Track Record Length}
\usage{
  MinTrackRecord(R = NULL, refSR, Rf = 0, p = 0.95,
    weights = NULL, sr = NULL, sk = NULL, kr = NULL, ...)
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

  \item{sr}{Sharpe Ratio,in the same periodicity as the
  returns(non-annualized). To be given in case the return
  series is not given.}

  \item{sk}{Skewness, in the same periodicity as the
  returns(non-annualized). To be given in case the return
  series is not given.}

  \item{kr}{Kurtosis, in the same periodicity as the
  returns(non-annualized). To be given in case the return
  series is not given.}

  \item{\dots}{any other passthru variable}
}
\description{
  Minimum Track Record Length tells us "How long should a
  track record be in order to have statistical confidence
  that its Sharpe ratio is above a given threshold? ". If a
  track record is shorter than MinTRL, we do not have
  enough confidence that the observed Sharpe Ratio is above
  the designated threshold. The reference Sharpe Ratio
  should be less than the observed Sharpe Ratio and the
  Values should be given in non-annualized terms, in the
  same periodicity as the return series. The Minimum Track
  Record Length is also given in the same Periodicity as
  the Return Series.

  \deqn{MinTRL = n^\ast =
  1+\biggl[1-\hat{\gamma_3}\hat{SR}+\frac{\hat{\gamma_4}}{4}\hat{SR^2}\biggr]\biggl(\frac{Z_\alpha}{\hat{SR}-SR^\ast}\biggr)^2}

  \eqn{\gamma{_3}} and \eqn{\gamma{_4}} are the skewness
  and kurtosis respectively. It is important to note that
  MinTRL is expressed in terms of number of observations,
  not annual or calendar terms.

  The sharpe ratio , skewness and kurtosis can be directly
  given if the return series is not available using the
  input parameters sr,sk and kr. If the return series is
  available these parameters can be left.

  weights will be needed to be entered if a portfolio's
  MinTRL is to be calculated else weight can be left as
  NULL.
}
\examples{
data(edhec)
MinTrackRecord(edhec[,1],refSR=0.1,Rf = 0.04/12)
MinTrackRecord(refSR = 1/12^0.5,Rf = 0,p=0.95,sr = 2/12^0.5,sk=-0.72,kr=5.78)
MinTrackRecord(edhec[,1:2],refSR = c(0.28,0.24))
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
  \code{\link{ProbSharpeRatio}} \code{\link{PsrPortfolio}}
  \code{\link{table.PSR}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

