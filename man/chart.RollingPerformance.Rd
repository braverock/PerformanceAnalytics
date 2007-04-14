\name{chart.RollingPerformance}
\alias{chart.RollingPerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to create a chart of rolling performance metrics in a line chart }
\description{
A wrapper to create a chart of rolling performance metrics in a line chart
}
\usage{
chart.RollingPerformance(R, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), FUN = "Return.annualized", na.pad = TRUE, type = "l", pch = NULL, lty = 1, bg = NULL, cex = 1, lwd = 1, xlim = NULL, ylim = NULL, log = "", main = paste(width,"-Month Rolling Performance", sep=""), sub = NULL, xlab = NULL, ylab = NULL, ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, panel.last = NULL, asp = NA, ylog = FALSE, event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color = "darkgray", period.color = "lightgray", darken = FALSE , date.format = "%m/%y", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{width}{ number of periods to apply rolling function window over }
  \item{xaxis}{ if true, draws the x axis }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{FUN}{ any function that can be evaluated using a single set of returns (e.g., rolling \code{\link{CAPM.beta}} won't work, but \code{\link{Return.annualized}} will) }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{na.pad}{ TRUE/FALSE If TRUE it adds any times that would not otherwise have been in the result with a value of NA. If FALSE those times are dropped. }
  \item{\dots}{ any other passthru parameters }
}
\value{
A timeseries line chart of the calculated series
}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link{charts.RollingPerformance}}
}
\examples{
data(edhec)
chart.RollingPerformance(edhec[, 1:3], width = 24)
chart.RollingPerformance(edhec[, 1:3], width = 24, colorset = rich8equal, lwd = 2, legend.loc = "topleft")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
