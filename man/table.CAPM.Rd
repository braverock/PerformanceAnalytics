\name{table.CAPM}
\alias{table.CAPM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Asset-Pricing Model Summary: Statistics and Stylized Facts }
\description{
Takes a set of returns and relates them to a market benchmark. Provides a set of measures related to the excess return single index model, or CAPM.
}
\usage{
table.CAPM(Ra, Rb, scale = NA, Rf = 0, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector of returns to test, e.g., the asset to be examined }
  \item{Rb}{ a matrix, data.frame, or timeSeries of benchmark(s) to test the asset against. }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{digits}{ number of digits to round results to }
}
\details{ This table will show statistics pertaining to an asset against a set of benchmarks, or statistics for a set of assets against a benchmark. }
\value{ A data frame with columns named for the columns in \code{Rb} and rows containing statistics.

}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link{CAPM.alpha}} \cr
    \code{\link{CAPM.beta}}  \cr
    \code{\link{TrackingError}}  \cr
    \code{\link{ActivePremium}}  \cr
    \code{\link{InformationRatio}}  \cr
    \code{\link{TreynorRatio}}
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
time(data.zoo) = as.Date(time(data.zoo),format="\%b \%Y")
time(rf.zoo) = as.Date(time(rf.zoo),format="\%b \%Y")

table.CAPM(data.zoo[,1,drop=FALSE], data.zoo[,2,drop=FALSE], rf = rf.zoo)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
