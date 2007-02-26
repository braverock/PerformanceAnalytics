\name{chart.Scatter}
\alias{chart.Scatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to draw scatter plot with sensible defaults }
\description{
Draws a scatter chart.  This is another chart "primitive", since it only contains a set of sensible defaults.  This function is intended to be used in a wrapper that is written for a particular purpose. This is just a handy way to standardize the formatting of multiple charts.
}
\usage{
chart.Scatter(x, y, reference.grid = TRUE, main = "Title", ylab = NULL, xlab = NULL, xlim = NA, ylim = NA, colorset = (1:12), symbolset = 1, darken = FALSE, legend.loc = NULL, ylog = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data for the x axis, can take matrix,vector, or timeseries }
  \item{y}{ data for the y axis, can take matrix,vector, or timeseries }
  \item{reference.grid}{ if true, draws a grid aligned with the points on the x and y axes }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{ylab}{ set the y-axis label, as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, as in \code{\link{plot}} }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{symbolset}{ from \code{pch} in \code{\link{plot}},  submit a set of symbols to be used in the same order as the data sets submitted }
  \item{darken}{ if true, draws the chart elements in "darkgray" rather than "gray".  Makes it easier to print for some printers. }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{ylog}{ ~~Describe \code{ylog} here~~ }
  \item{\dots}{ any other passthru parameters }
}
\value{
Draws a graph of type "scatter" with some sensible defaults.
}
\references{ }
\author{ Peter Carl }
\note{
Most inputs are the same as "\code{\link{plot}}" and are principally included so that some sensible defaults could be set.
}
\seealso{ \code{\link{plot}} }
\examples{
data(edhec)
chart.Scatter(edhec[,1],edhec[,2])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
