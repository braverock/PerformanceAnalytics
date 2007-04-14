\name{findDrawdowns}
\alias{findDrawdowns}
\alias{Drawdowns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Find the drawdowns and drawdown levels in a timeseries. }
\description{
\code{findDrawdowns} will find the starting period, the ending period, and the amount and length of the drawdown.

Often used with \code{\link{sortDrawdowns}} to get the largest drawdowns.

\code{Drawdowns} will calculate the drawdown levels as percentages, for use in \code{\link{chart.Drawdown}}.
}
\usage{
findDrawdowns(R)
Drawdowns(R)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
}
\value{
  unordered list \cr
  \item{return }{depth of drawdown} \cr
  \item{from }{starting period} \cr
  \item{to }{ending period} \cr
  \item{length }{length in periods} \cr
}
\references{}
\author{
Peter Carl

\code{findDrawdowns} modified with permission from function by Sankalp Upadhyay
}
\seealso{
    \code{\link{sortDrawdowns}}
    \code{\link{maxDrawdown}}
    \code{\link{chart.Drawdown}}
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