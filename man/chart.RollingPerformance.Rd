\name{chart.RollingPerformance}
\alias{chart.RollingPerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to create a chart of rolling performance metrics in a line chart }
\description{
A wrapper to create a chart of rolling performance metrics in a line chart
}
\usage{
chart.RollingPerformance(R, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), FUN = "Return.annualized", na.pad = TRUE, type = "l", pch = NULL, lty = 1, bg = NULL, cex = 1, lwd = 1, xlim = NULL, ylim = NULL, log = "", main = paste(width,"-Month Rolling Performance", sep=""), sub = NULL, xlab = NULL, ylab = NULL, ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, panel.last = NULL, asp = NA, ylog = FALSE, event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color = "darkgray", period.color = "lightgray", darken = FALSE , date.format = "\%m/\%y", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{width}{ number of periods to apply rolling function window over }
  \item{FUN}{ any function that can be evaluated using a single set of returns (e.g., rolling \code{\link{CAPM.beta}} won't work, but \code{\link{Return.annualized}} will) }
  \item{na.pad}{ TRUE/FALSE If TRUE it adds any times that would not otherwise have been in the result with a value of NA. If FALSE those times are dropped. }
  \item{ylog}{ TRUE/FALSE set the y-axis to logarithmic scale, similar to \code{\link{plot}}, default FALSE }
  \item{date.format}{ re-format the dates for the xaxis; the default is "\%m/\%y" }
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
  \item{darken}{ if true, draws the chart elements in "darkgray" rather than "gray".  Makes it easier to print for some printers. }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{bg}{ ~~Describe~~ }
  \item{cex}{ ~~Describe~~ }
  \item{log}{ ~~Describe~~ }
  \item{sub}{ ~~Describe~~ }
  \item{ann}{ ~~Describe~~ }
  \item{axes}{ ~~Describe~~ }
  \item{frame.plot}{ ~~Describe~~ }
  \item{panel.first}{ ~~Describe~~ }
  \item{panel.last}{ ~~Describe~~ }
  \item{asp}{ ~~Describe~~ }
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
