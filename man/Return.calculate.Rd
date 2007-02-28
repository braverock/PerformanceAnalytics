\name{CalculateReturns}
\alias{CalculateReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate simple or compound returns from prices }
\description{
calculate simple or compound returns from prices
}
\usage{
CalculateReturns(Prices, method=c("simple","compound"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Prices}{vector or matrix of prices}
  \item{method}{calculate "simple" or "compound" returns}
}
\value{
vector or matrix of simple or compound returns
}
\details{
}
\references{ }
\author{ Peter Carl }
\note{
}
\seealso{ }
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