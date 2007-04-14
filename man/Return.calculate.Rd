\name{CalculateReturns}
\alias{CalculateReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate simple or compound returns from prices }
\description{
calculate simple or compound returns from prices
}
\usage{
CalculateReturns(prices, method=c("compound","simple"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prices}{ data object containing ordered price observations }
  \item{method}{ calculate "simple" or "compound" returns, default compound }
}
\value{
vector or matrix of simple or compound returns
}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link{Return.cumulative}}
}
\examples{
require(tseries)
prices = get.hist.quote("^gspc", start = "1999-01-01", end = "2007-01-01", quote = "Close", compression = "m")
returns = CalculateReturns(prices, method="simple")
colnames(returns)="SP500"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }