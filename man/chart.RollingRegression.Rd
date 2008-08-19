\name{chart.RollingRegression}
\alias{chart.RollingRegression}
\alias{charts.RollingRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A wrapper to create charts of relative regression performance through time }
\description{
A wrapper to create a chart of relative regression performance through time

A group of charts in \code{charts.RollingRegression} displays alpha, beta, and R-squared estimates in three aligned charts in a single device.
}
\usage{
chart.RollingRegression(Ra, Rb, width = 12, rf = 0,
                 attribute = c("Beta","Alpha", "R-Squared"),
                 main = paste("Rolling ", width,"-Month ", attribute, sep = ""),
                 xaxis = TRUE, colorset = (1:12), legend.loc = NULL,
                 na.pad = TRUE, ...)

charts.RollingRegression(Ra, Rb, width = 12, rf = 0, darken = FALSE,
                 main = NULL, legend.loc = NULL, event.labels = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{width}{ number of periods to apply rolling function window over }
  \item{attribute}{ one of "Beta","Alpha","R-Squared" for which attribute to show }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{event.labels}{ TRUE/FALSE whether or not to display lines and labels for historical market shock events }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{xaxis}{ if true, draws the x axis }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{darken}{ TRUE/FALSE whether or not to darken the color palette for better printing on some printers }
  \item{na.pad}{ TRUE/FALSE If TRUE it adds any times that would not otherwise have been in the result with a value of NA. If FALSE those times are dropped. }
  \item{\dots}{ any other passthru parameters }
}
\details{
The attribute parameter is probably the most confusing.  In mathematical terms, the different choices yeild the following:

Alpha - shows the y-intercept\cr
Beta - shows the slope of the regression line\cr
R-Squared - shows the degree of fit of the regression to the data\cr

}
\value{
A timeseries line chart of the calculated series
}
\references{ }
\author{ Peter Carl }
\note{
Most inputs are the same as "\code{\link{plot}}" and are principally included so that some sensible defaults could be set.
}
\seealso{}
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

chart.RollingRegression(data.zoo[, 1, drop=FALSE], data.zoo[, 2, drop=FALSE], rf = rf.zoo)
charts.RollingRegression(data.zoo[, 1, drop=FALSE], data.zoo[, 2, drop=FALSE], rf = rf.zoo)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
