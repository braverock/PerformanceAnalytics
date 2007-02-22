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
Peter Carl
modified with permission from function by Sankalp Upadhyay
}
\seealso{
    \code{\link{findDrawdowns}}
    \code{\link{maxDrawdown}}
}
\examples{
data(edhec)
R=edhec[,"Funds.of.Funds",drop=FALSE]
findDrawdowns(R)
sortDrawdowns(findDrawdowns(R))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }