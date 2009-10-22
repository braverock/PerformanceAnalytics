\name{chart.RollingPerformance}
\alias{chart.RollingPerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to create a chart of rolling performance metrics in a line chart }
\description{
A wrapper to create a chart of rolling performance metrics in a line chart
}
\usage{
chart.RollingPerformance(R, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), FUN = "Return.annualized", na.pad = TRUE, type = "l", pch = NULL, lty = 1, bg = NULL, cex.axis=0.8, cex.legend = 0.8, cex.labels = 0.7, lwd = 2, xlim = NULL, ylim = NULL, log = "", main = NULL, sub = NULL, xlab = "Date", ylab = NULL, ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, panel.last = NULL, asp = NA, ylog = FALSE, event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color = "darkgray", period.color = "lightgray", element.color = "darkgray", major.ticks='auto', minor.ticks=TRUE, grid.color="lightgray", grid.lty="dotted", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{width}{ number of periods to apply rolling function window over }
  \item{FUN}{ any function that can be evaluated using a single set of returns (e.g., rolling \code{\link{CAPM.beta}} won't work, but \code{\link{Return.annualized}} will) }
  \item{na.pad}{ TRUE/FALSE If TRUE it adds any times that would not otherwise have been in the result with a value of NA. If FALSE those times are dropped. }
  \item{ylog}{ TRUE/FALSE set the y-axis to logarithmic scale, similar to \code{\link{plot}}, default FALSE }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{event.lines}{ If not null, vertical lines will be drawn to indicate that an event happened during that time period.  \code{event.lines} should be a list of dates (e.g., \code{c("09/03","05/06"))} formatted the same as date.format.  This function matches the re-formatted row names (dates) with the events.list, so to get a match the formatting needs to be correct. }
  \item{event.labels}{ if not null and event.lines is not null, this will apply a list of text labels (e.g., \code{c("This Event", "That Event")} to the vertical lines drawn }
  \item{event.color}{ draws the event described in \code{event.labels} in the color specified }
  \item{period.areas}{ these are shaded areas described by start and end dates in the same format as the date.format.  This is provided as a list of pairs, e.g., \code{list(c("10/26","11/27"), c("08/29","03/33"))} See the examples below. }
  \item{period.color}{ draws the shaded region described by \code{period.areas} in the color specified }
  \item{type}{ set the chart type, same as in \code{\link{plot}} }
  \item{xaxis}{ if true, draws the x axis }
  \item{ylab}{ set the y-axis label, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{pch}{ symbols to use, see also \code{\link{plot}} }
  \item{lty}{ set the line type, same as in \code{\link{plot}} }
  \item{lwd}{ set the line width, same as in \code{\link{plot}} }

  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{bg}{ same as in \code{\link{plot}} }
  \item{cex}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{log}{ not used }
  \item{sub}{ same as in \code{\link{plot}} }
  \item{ann}{ same as in \code{\link{plot}} }
  \item{axes}{ same as in \code{\link{plot}} }
  \item{frame.plot}{ same as in \code{\link{plot}} }
  \item{panel.first}{ same as in \code{\link{plot}} }
  \item{panel.last}{ same as in \code{\link{plot}} }
  \item{asp}{ same as in \code{\link{plot}} }
  \item{cex.axis}{ The magnification to be used for axis annotation relative to the current setting of 'cex', same as in \code{\link{plot}}.  }
  \item{cex.legend}{ The magnification to be used for sizing the legend relative to the current setting of 'cex'.  }
  \item{cex.labels}{ The magnification to be used for event line labels relative to the current setting of 'cex'.  }
  \item{major.ticks}{ Should major tickmarks be drawn and labeled, default 'auto'}
  \item{minor.ticks}{ Should minor tickmarks be drawn, default TRUE}
  \item{grid.color}{ sets the color for the reference grid }
  \item{grid.lty}{ defines the line type for the grid }
  \item{element.color}{ provides the color for drawing chart elements, such as the box lines, axis lines, etc. Default is "darkgray" } 
  \item{\dots}{ any other passthru parameters to \code{\link[zoo]{rollapply}} }
}
\value{
A timeseries line chart of the calculated series
}
\author{ Peter Carl }
\seealso{
    \code{\link{charts.RollingPerformance}}, \code{\link[zoo]{rollapply}}
}
\examples{
data(edhec)
chart.RollingPerformance(edhec[, 1:3], width = 24)
chart.RollingPerformance(edhec[, 1:3], FUN = 'mean', width = 24, colorset = rich8equal, lwd = 2, legend.loc = "topleft", main = "Rolling 24-Month Mean Return")
chart.RollingPerformance(edhec[, 1:3], FUN = 'SharpeRatio.annualized', width = 24, colorset = rich8equal, lwd = 2, legend.loc = "topleft", main = "Rolling 24-Month Sharpe Ratio")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
