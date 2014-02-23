\name{sortDrawdowns}
\alias{sortDrawdowns}
\title{order list of drawdowns from worst to best}
\usage{
sortDrawdowns(runs)
}
\arguments{
  \item{runs}{pass in runs array from findDrawdowns to be
  sorted}
}
\description{
sortDrawdowns(findDrawdowns(R)) Gives the drawdowns in
order of worst to best
}
\details{
Returns a sorted list: \itemize{ \item return depth of
drawdown \item from starting period \item to ending period
\item length length in periods }
}
\examples{
data(edhec)
findDrawdowns(edhec[,"Funds of Funds", drop=FALSE])
sortDrawdowns(findDrawdowns(edhec[,"Funds of Funds", drop=FALSE]))
}
\author{
Peter Carl \cr modified with permission from prototype
function by Sankalp Upadhyay
}
\references{
Bacon, C. \emph{Practical Portfolio Performance Measurement
and Attribution}. Wiley. 2004. p. 88 \cr
}
\seealso{
\code{\link{DownsideDeviation}} \cr
\code{\link{maxDrawdown}} \cr \code{\link{findDrawdowns}}
\cr \code{\link{sortDrawdowns}} \cr
\code{\link{chart.Drawdown}} \cr
\code{\link{table.Drawdowns}} \cr
\code{\link{table.DownsideRisk}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

