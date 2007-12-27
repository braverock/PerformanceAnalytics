\name{chart.RelativePerformance}
\alias{chart.RelativePerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ relative performance chart between multiple return series }
\description{
A wrapper to create a chart of relative returns through time
}
\usage{
chart.RelativePerformance(Ra, Rb, main = "Relative Performance", xaxis = TRUE,
                 colorset = (1:12), legend.loc = NULL, ylog = FALSE,
                 elementcolor = "darkgray", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{xaxis}{ if true, draws the x axis }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{ylog}{ TRUE/FALSE set the y-axis to logarithmic scale, similar to \code{\link{plot}}, default FALSE }
  \item{\dots}{ any other passthru parameters }
}
\value{
chart of relative returns
}
\references{ }
\author{ Peter Carl }
\seealso{ }
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
chart.RelativePerformance(data.zoo[, 1, drop=FALSE], data.zoo[, 2, drop=FALSE], colorset=rich8equal, legend.loc="bottomright", lwd=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
