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
chart.RollingRegression(R, Rb, n = 12, rf = 0, attribute = c("Beta","Alpha","R-Squared"), main = paste("Rolling ", n, "-Month ", attribute, sep = ""), xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ...)
charts.RollingRegression(R, Rb, n = 12, rf = 0, darken = FALSE, main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{n}{ number of periods to apply rolling function over }
  \item{attribute}{ one of "Beta","Alpha","R-Squared" for which attribute to show }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{xaxis}{ if true, draws the x axis }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{darken}{ TRUE/FALSE whether or not to darken the color palette for better printing on some printers }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{\dots}{ any other passthru parameters }
}
\details{
}
\value{
A timeseries line chart of the calculated series
}
\references{ }
\author{ Peter Carl }
\note{
Most inputs are the same as "\code{\link{plot}}" and are principally included so that some sensible defaults could be set.
}
\seealso{
   \code{\link{chart.RegressionDiagnostics}}
}
\examples{
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
sp500 = download.SP500PriceReturns(start="1996-12-31",end=end)
rf = download.RiskFree(start=start, end=end)

chart.RollingRegression(edhec[,"Long.Short.Equity",drop=FALSE],sp500,rf=rf)
chart.RollingRegression(edhec[,"Long.Short.Equity",drop=FALSE],sp500,rf=rf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
