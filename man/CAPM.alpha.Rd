\name{CAPM.alpha}
\alias{CAPM.alpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate CAPM alpha }
\description{
This is a wrapper for calculating a CAPM alpha.

"Alpha" purports to be a measure of a manager's skill by measuring the portion of the managers returns that are not attributable to "Beta", or the performance relative to a benchmark.
}
\usage{
CAPM.alpha(R, Rb, rf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{rf}{ risk free rate, in same period as your returns }
}
\value{
CAPM alpha
}
\references{ ~put references to the literature/web site here ~ }
\author{ Peter Carl }
\note{}
\seealso{
    \code{\link{CAPM.beta}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }