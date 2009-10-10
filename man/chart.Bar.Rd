\name{chart.Bar}
\alias{chart.Bar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper for barchart of returns }
\description{
A wrapper to create a chart of periodic returns in a bar chart.  This is a difficult enough graph to read that it doesn't get much use.  Still, it is useful for viewing a single set of data.
}
\usage{
    chart.Bar(R, legend.loc = NULL, colorset = (1:12), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{\dots}{ any other passthru parameters, see \code{plot} }
}
\details{
This is really a wrapper for chart.TimeSeries, so several other attributes can also be passed.
}
\value{
Creates a plot of time on the x-axis and vertical lines for each period to indicate value on the y-axis.
}
\references{ }
\author{ Peter Carl }
\
\seealso{
    \code{\link{chart.TimeSeries}} \cr
    \code{\link{plot}}
}
\examples{
data(edhec)
chart.Bar(edhec[,"Funds.of.Funds"], main="Monthly Returns")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }