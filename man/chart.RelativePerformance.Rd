\name{chart.RelativePerformance}
\alias{chart.RelativePerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ relative performance chart between multiple return series }
\description{
A wrapper to create a chart of relative returns through time
}
\usage{
chart.RelativePerformance(R, Rb, main = "Relative Performance", xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ylog = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{xaxis}{ if true, draws the x axis }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{ylog}{ ~~Describe \code{ylog} here~~ }
  \item{\dots}{ any other passthru parameters }
}
\value{
chart of relative returns
}
\references{ }
\author{ Peter Carl }
\seealso{ }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
