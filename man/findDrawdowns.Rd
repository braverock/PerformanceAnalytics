\name{findDrawdowns}
\alias{findDrawdowns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Find the drawdowns in a timeseries. }
\description{
Find the starting period, the ending period, and the amount and length of the drawdown.

Often used with \code{\link{sortDrawdowns}} to get the largest drawdowns.
}
\usage{
findDrawdowns(R)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
}
\value{
  unordered list
  \item{return }{depth of drawdown}
  \item{from }{starting period}
  \item{to }{ending period}
  \item{length }{length in periods}
}
\references{}
\author{
Peter Carl
modified with permission from function by Sankalp Upadhyay
}
\note{
}
\seealso{
    \code{\link{sortDrawdowns}}
    \code{\link{maxDrawdown}}
}
\examples{
data(edhec)
R=edhec[,"Funds.of.Funds"]
findDrawdowns(R)
sortDrawdowns(findDrawdowns(R))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }