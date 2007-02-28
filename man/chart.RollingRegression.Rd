\name{chart.RollingRegression}
\alias{chart.RollingRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A wrapper to create a chart of relative returns through time }
\description{
A wrapper to create a chart of relative returns through time
}
\usage{
chart.RollingRegression(R, Rb, n = 12, rf = 0, attribute = "Beta", main = paste("Rolling ", n, "-Month ", attribute, sep = ""), xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{n}{ number of periods to apply rolling function over }
  \item{attribute}{ ~~Describe \code{attribute} here~~ }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{xaxis}{ if true, draws the x axis }
  \item{colorset}{ color palette to use, set by default to rational choices }
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
   \code{\link{charts.RollingRegression}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
