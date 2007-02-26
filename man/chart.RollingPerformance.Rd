\name{chart.RollingPerformance}
\alias{chart.RollingPerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to create a chart of rolling performance metrics in a line chart }
\description{
A wrapper to create a chart of rolling performance metrics in a line chart
}
\usage{
chart.RollingPerformance(R, n = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), FUN = "Return.annualized", rf = 0, trim = FALSE, na.rm = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{n}{ number of periods to apply rolling function over }
  \item{xaxis}{ if true, draws the x axis }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{FUN}{ any function that can be evaluated using a single set of returns \cr
     \tab (e.g., rolling \code{\link{CAPM.beta}} won't work, but \code{\link{Return.annualized}} will)
  }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{\dots}{ any other passthru parameters }
}
\value{
A timeseries line chart of the calculated series
}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link{charts.RollingPerformance}}, ~~~ }
\examples{
data(edhec)
chart.RollingPerformance(edhec[,1:3], n=24)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
