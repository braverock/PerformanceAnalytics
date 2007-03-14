\name{chart.RollingCorrelation}
\alias{chart.RollingCorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ chart rolling correlation fo multiple assets }
\description{
A wrapper to create a chart of rolling correlation metrics in a line chart
}
\usage{
chart.RollingCorrelation(R, Rb, n = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), trim = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{n}{ number of periods to apply rolling function over }
  \item{xaxis}{ if true, draws the x axis }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
