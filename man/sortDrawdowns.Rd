\name{sortDrawdowns}
\alias{sortDrawdowns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ order list of drawdowns from worst to best }
\description{
sortDrawdowns(findDrawdowns(R))
Gives the drawdowns in order of worst to best
}
\usage{
sortDrawdowns(runs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{runs}{ pass in runs array from findDrawdowns to be sorted }
}
\value{
  sorted list
  \item{return }{depth of drawdown}
  \item{from }{starting period}
  \item{to }{ending period}
  \item{length }{length in periods}
}
\references{}
\author{
Peter Carl \cr
modified with permission from prototype function by Sankalp Upadhyay
}
\seealso{
    \code{\link{DownsideDeviation}}  \cr
    \code{\link{maxDrawdown}}   \cr
    \code{\link{findDrawdowns}} \cr
    \code{\link{sortDrawdowns}} \cr
    \code{\link{chart.Drawdown}} \cr
    \code{\link{table.Drawdowns}} \cr
    \code{\link{table.DownsideRisk}}
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