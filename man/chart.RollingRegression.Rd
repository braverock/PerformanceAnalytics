\name{chart.RollingRegression}
\alias{chart.RollingRegression}
\alias{chart.RollingQuantileRegression}
\alias{charts.RollingRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A wrapper to create charts of relative regression performance through time }
\description{
A wrapper to create a chart of relative regression performance through time

A group of charts in \code{charts.RollingRegression} displays alpha, beta, and R-squared estimates in three aligned charts in a single device.
}
\usage{ 
chart.RollingRegression(Ra, Rb, width = 12, Rf = 0, attribute = c("Beta", "Alpha", "R-Squared"), main=NULL, na.pad = TRUE, ...)
chart.RollingQuantileRegression(Ra, Rb, width = 12, Rf = 0, attribute = c("Beta", "Alpha", "R-Squared"), main=NULL, na.pad = TRUE, ...)

charts.RollingRegression(Ra, Rb, width = 12, Rf = 0, main = NULL, legend.loc = NULL, event.labels = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{width}{ number of periods to apply rolling function window over }
  \item{attribute}{ one of "Beta","Alpha","R-Squared" for which attribute to show }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{event.labels}{ TRUE/FALSE whether or not to display lines and labels for historical market shock events }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{na.pad}{ TRUE/FALSE If TRUE it adds any times that would not otherwise have been in the result with a value of NA. If FALSE those times are dropped. }
  \item{\dots}{ any other passthru parameters to \code{\link{chart.TimeSeries}} }
}
\details{
The attribute parameter is probably the most confusing.  In mathematical terms, the different choices yeild the following:

Alpha - shows the y-intercept\cr
Beta - shows the slope of the regression line\cr
R-Squared - shows the degree of fit of the regression to the data\cr

\code{chart.RollingQuantileRegression} uses \code{\link[quantreg]{rq}} rather than
\code{\link[stats]{lm}} for the regression, and may be more robust to outliers in the data.
}
\value{
A timeseries line chart of the calculated series
}
\author{ Peter Carl }
\note{
Most inputs are the same as "\code{\link{plot}}" and are principally included so that some sensible defaults could be set.
}
\seealso{
\code{\link[stats]{lm}} \cr
\code{\link[quantreg]{rq}} \cr
}
\examples{
# First we load the data
data(managers)
chart.RollingRegression(managers[, 1, drop=FALSE], managers[, 8, drop=FALSE], Rf = .04/12)
charts.RollingRegression(managers[, 1:6], managers[, 8, drop=FALSE], Rf = .04/12, colorset = rich6equal, legend.loc="topleft")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
