\name{TrackingError}
\alias{TrackingError}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate Tracking Error of returns against a benchmark }
\description{
A measure of the unexplained portion of performance relative to a benchmark.
}
\usage{
TrackingError(Ra, Rb, scale = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\details{
Tracking error is calculated by taking the square root of the average of the squared deviations between the investment's returns and the benchmark's returns, then multiplying the result by the square root of the scale of the returns.

\deqn{ TrackingError = \sqrt{\sum\frac{(R_{a}-R_{b})^{2}}{len(R_{a})\sqrt{scale}}} }{ TrackingError = sqrt(sum(Ra - Rb)^2 / (length(Ra) - 1)) * sqrt(scale)}
}
\value{
Tracking Error (number)
}
\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.
}
\author{ Peter Carl }
\note{
}
\seealso{
    \code{\link{InformationRatio}}
    \code{\link{TrackingError}}
}
\examples{
# First we load the data
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
edhec.zoo = zoo(edhec, order.by = rownames(edhec))
sp500.zoo = download.SP500PriceReturns(start = "1996-12-31", end = end)

# Now we have to align it as "monthly" data
time(edhec.zoo) = as.yearmon(time(edhec.zoo))
time(sp500.zoo) = as.yearmon(time(sp500.zoo))
data.zoo = merge(edhec.zoo[,9,drop=FALSE],sp500.zoo)
time(data.zoo) = as.Date(time(data.zoo),format="\%b \%Y")

TrackingError(data.zoo[, 1, drop=FALSE], data.zoo[, 2, drop=FALSE])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
