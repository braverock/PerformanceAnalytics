\name{CAPM.alpha}
\alias{CAPM.alpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate CAPM alpha }
\description{
This is a wrapper for calculating a CAPM alpha.

"Alpha" purports to be a measure of a manager's skill by measuring the portion of the managers returns that are not attributable to "Beta", or the performance relative to a benchmark.
}
\usage{
CAPM.alpha(Ra, Rb, rf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{rf}{ risk free rate, in same period as your returns }
}
\value{
CAPM alpha
}
\references{
Sharpe, W.F. Capital Asset Prices: A theory of market equilibrium under conditions of risk. \emph{Journal of finance}, vol 19, 1964, 425-442. \cr
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004. \cr
}
\author{ Peter Carl }
\note{}
\seealso{
    \code{\link{CAPM.beta}}
    \code{\link{CAPM.utils}}
}
\examples{
# First we load the data
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
edhec.zoo = zoo(edhec, order.by = rownames(edhec))
rf.zoo = download.RiskFree(start = start, end = end)
sp500.zoo = download.SP500PriceReturns(start = "1996-12-31", end = end)

# Now we have to align it as "monthly" data
time(edhec.zoo) = as.yearmon(time(edhec.zoo))
time(sp500.zoo) = as.yearmon(time(sp500.zoo))
time(rf.zoo) = as.yearmon(time(rf.zoo))
data.zoo = merge(edhec.zoo[,9,drop=FALSE],sp500.zoo)
time(data.zoo) = as.Date(time(data.zoo),format="%b %Y")
time(rf.zoo) = as.Date(time(rf.zoo),format="%b %Y")

CAPM.alpha(data.zoo[, 1, drop=FALSE], data.zoo[, 2, drop=FALSE], rf = rf.zoo)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }