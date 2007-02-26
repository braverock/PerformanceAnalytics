\name{StdDev}
\alias{StdDev}
\alias{std}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Standard Deviation of Monthly Returns }
\description{
Standard Deviation of Monthly Returns

std = sqrt(var(R))

May be annualized using \code{\link{StdDev.annualized}}
}
\usage{
StdDev(R)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
}
\value{
standard deviation
}
\note{
This function is really just a wrapper.  It exists because there are several examples for R, S-Plus, and other languages that call standard deviation StdDev, std, or sd.  This makes things a little easier to port and reuse.
}
\references{}
\author{ Brian G. Peterson }
\seealso{
    \code{\link{StdDev.annualized}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }