\name{CAPM.alpha}
\alias{CAPM.alpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate CAPM alpha }
\description{
This is a wrapper for calculating a CAPM alpha.

"Alpha" purports to be a measure of a manager's skill by measuring the portion of the managers returns that are not attributable to "Beta", or the performance relative to a benchmark.
}
\usage{
CAPM.alpha(Ra, Rb, Rf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{Rf}{ risk free rate, in same period as your returns }
}
\value{
CAPM alpha
}
\references{
Sharpe, W.F. Capital Asset Prices: A theory of market equilibrium under conditions of risk. \emph{Journal of finance}, vol 19, 1964, 425-442. \cr
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004. \cr
}
\author{ Peter Carl }
\seealso{
    \code{\link{CAPM.beta}}
    \code{\link{CAPM.utils}}
}
\examples{
# First we load the data
data(managers)
CAPM.alpha(managers[, "HAM1", drop=FALSE], managers[, "SP500.TR", drop=FALSE], rf = managers[, "US.3m.TR", drop=FALSE])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }