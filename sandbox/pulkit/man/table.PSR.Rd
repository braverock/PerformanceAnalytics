\name{table.PSR}
\alias{table.PSR}
\title{Probabilistic Sharpe Ratio}
\usage{
  table.PSR(R = NULL, refSR, Rf = 0, p = 0.95,
    weights = NULL, ...)
}
\arguments{
  \item{R}{the return series}

  \item{Rf}{the risk free rate of return}

  \item{refSR}{the reference Sharpe Ratio}

  \item{p}{the confidence level}

  \item{weights}{the weights for the portfolio}

  \item{\dots}{Any other passthru variable}
}
\description{
  A table to display the Probabilistic Sharpe Ratio Along
  with the Minimum Track Record Length for better
  assessment of the returns.For more details about
  Probabilistic Sharpe Ratio and Minimum Track record
  length see\ \code{ProbSharpeRatio} and
  \code{MinTrackRecord} respectively.
}
\examples{
data(edhec)
table.PSR(edhec[,1],0.20)
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
  \code{\link{MinTrackRecord}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

