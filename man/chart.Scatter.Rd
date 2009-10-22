\name{chart.Scatter}
\alias{chart.Scatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to draw scatter plot with sensible defaults }
\description{
Draws a scatter chart.  This is another chart "primitive", since it only contains a set of sensible defaults.
}
\usage{
chart.Scatter(x, y, reference.grid = TRUE, main = "Title", ylab = NULL, xlab = NULL, xlim = NA, ylim = NA, colorset = 1, symbolset = 1, element.color = "darkgray", cex.axis = 0.8, cex.legend = 0.8, cex.lab = 1, cex.main = 1, ...)
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
  \item{element.color}{ provides the color for drawing chart elements, such as the box lines, axis lines, etc. Default is "darkgray" }
  \item{cex.lab}{ The magnification to be used for x- and y-axis labels relative to the current setting of 'cex'}
  \item{cex.axis}{ The magnification to be used for axis annotation relative to the current setting of 'cex', same as in \code{\link{plot}}.  }
  \item{cex.main}{ The magnification to be used for the main title relative to the current setting of 'cex'.  }  \item{cex.legend}{ The magnification to be used for sizing the legend relative to the current setting of 'cex'.  }
  \item{\dots}{ any other passthru parameters }
}
\value{
Draws a graph of type "scatter" with some sensible defaults.
}
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
