\name{TreynorRatio}
\alias{TreynorRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate Treynor Ratio of excess return over CAPM beta }
\description{
The Treynor ratio is similar to the Sharpe Ratio, except it uses beta as the volatility measure (to divide the investment's excess return over the beta). }
\usage{
TreynorRatio(Ra, Rb, rf = 0, scale = 12, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{\dots}{ any other passthru parameters }
}
\value{
Treynor ratio
}
\details{
Equation:
\deqn{\frac{\overline{(R_{a}-R_{f})}}{\beta_{a,b}}}
}
\references{
}
\author{ Peter Carl }
\seealso{
   \code{\link{SharpeRatio}}
   \code{\link{SortinoRatio}}
   \code{\link{CAPM.beta}}
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

TreynorRatio(data.zoo[,1], data.zoo[,2], rf=rf.zoo)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }