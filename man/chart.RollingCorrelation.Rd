\name{chart.RollingCorrelation}
\alias{chart.RollingCorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ chart rolling correlation fo multiple assets }
\description{
A wrapper to create a chart of rolling correlation metrics in a line chart
}
\usage{
chart.RollingCorrelation(Ra, Rb, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), na.pad = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{width}{ number of periods to apply rolling function window over }
  \item{xaxis}{ if true, draws the x axis }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{na.pad}{ TRUE/FALSE If TRUE it adds any times that would not otherwise have been in the result with a value of NA. If FALSE those times are dropped. }
  \item{\dots}{ any other passthru parameters }
}
\details{
}
\value{
chart rolling period correlations of one or more assets
}
\references{ }
\author{ Peter Carl }
\note{}
\seealso{}
\examples{
# First we get the data
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])

edhec.zoo = zoo(edhec, order.by = rownames(edhec))
sp500.zoo = download.SP500PriceReturns(start = "1996-12-31", end = end)

# Then align the dates as "monthly" data
time(edhec.zoo) = as.yearmon(time(edhec.zoo))
time(sp500.zoo) = as.yearmon(time(sp500.zoo))
data.zoo = merge(edhec.zoo[,9,drop=FALSE],sp500.zoo)
time(data.zoo) = as.Date(time(data.zoo),format="\%b \%Y")

# Finally, plot it
chart.RollingCorrelation(data.zoo[, 1, drop=FALSE], data.zoo[, 2, drop=FALSE], colorset=rich8equal, legend.loc="bottomright", lwd=2, width=24)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
