\name{findDrawdowns}
\alias{Drawdowns}
\alias{findDrawdowns}
\title{Find the drawdowns and drawdown levels in a timeseries.}
\usage{
findDrawdowns(R, geometric = TRUE, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{geometric}{utilize geometric chaining (TRUE) or
  simple/arithmetic chaining (FALSE) to aggregate returns,
  default TRUE}

  \item{\dots}{any other passthru parameters}
}
\description{
\code{findDrawdowns} will find the starting period, the
ending period, and the amount and length of the drawdown.
}
\details{
Often used with \code{\link{sortDrawdowns}} to get the
largest drawdowns.

\code{Drawdowns} will calculate the drawdown levels as
percentages, for use in \code{\link{chart.Drawdown}}.

Returns an unordered list: \cr \itemize{ \item return depth
of drawdown \item from starting period \item to ending
period \item length length in periods }
}
\examples{
data(edhec)
findDrawdowns(edhec[,"Funds of Funds", drop=FALSE])
sortDrawdowns(findDrawdowns(edhec[,"Funds of Funds", drop=FALSE]))
}
\author{
Peter Carl

\code{findDrawdowns} modified with permission from function
by Sankalp Upadhyay
}
\references{
Bacon, C. \emph{Practical Portfolio Performance Measurement
and Attribution}. Wiley. 2004. p. 88 \cr
}
\seealso{
\code{\link{sortDrawdowns}} \cr \code{\link{maxDrawdown}}
\cr \code{\link{sortDrawdowns}} \cr
\code{\link{table.Drawdowns}} \cr
\code{\link{table.DownsideRisk}} \cr
\code{\link{chart.Drawdown}} \cr
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

