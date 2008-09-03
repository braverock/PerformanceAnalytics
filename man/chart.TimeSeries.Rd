\name{chart.TimeSeries}
\alias{chart.TimeSeries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Creates a time series chart with some extensions. }
\description{
Draws a line chart and labels the x-axis with the appropriate dates.  This is really a "primitive", since it extends the base \code{\link{plot}} and standardizes the elements of a chart.  Adds attributes for shading areas of the timeline or aligning vertical lines along the timeline.
}
\usage{
chart.TimeSeries(R, reference.grid = TRUE, xaxis = TRUE, yaxis = TRUE, yaxis.right = FALSE, type = "l", lty = 1, lwd = 1, main = NULL, ylab = NULL, xlab = "Date", date.format.in = "\%Y-\%m-\%d", date.format = "\%m/\%y", xlim = NULL, ylim = NULL, event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color  = "darkgray", period.color = "lightgray", colorset = (1:12), pch = (1:12), darken = FALSE, legend.loc =  NULL, ylog = FALSE, cex.axis = 0.8, cex.legend = 0.8, cex.labels = 0.8, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{reference.grid}{ if true, draws a grid aligned with the points on the x and y axes }
  \item{xaxis}{ if true, draws the x axis }
  \item{yaxis}{ if true, draws the y axis }
  \item{yaxis.right}{ if true, draws the y axis on the right-hand side of the plot }
  \item{type}{ set the chart type, same as in \code{\link{plot}} }
  \item{lty}{ set the line type, same as in \code{\link{plot}} }
  \item{lwd}{ set the line width, same as in \code{\link{plot}} }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{date.format}{ re-format the dates for the xaxis; the default is "\%m/\%y" }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{event.lines}{ If not null, vertical lines will be drawn to indicate that an event happened during that time period.  \code{event.lines} should be a list of dates (e.g., \code{c("09/03","05/06"))} formatted the same as date.format.  This function matches the re-formatted row names (dates) with the events.list, so to get a match the formatting needs to be correct. }
  \item{event.labels}{ if not null and event.lines is not null, this will apply a list of text labels (e.g., \code{c("This Event", "That Event")} to the vertical lines drawn.  See the example below. }
  \item{period.areas}{ these are shaded areas described by start and end dates in the same format as the date.format.  This is provided as a list of pairs, e.g., \code{list(c("10/26","11/27"), c("08/29","03/33"))} See the examples below. }
  \item{event.color}{ draws the event described in \code{event.labels} in the color specified }
  \item{period.color}{ draws the shaded region described by \code{period.areas} in the color specified }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{pch}{ symbols to use, see also \code{\link{plot}} }
  \item{darken}{ if true, draws the chart elements in "darkgray" rather than "gray".  Makes it easier to print for some printers. }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{ylog}{ TRUE/FALSE set the y-axis to logarithmic scale, similar to \code{\link{plot}}, default FALSE }
  \item{date.format.in}{ allows specification of other date formats in the data object, defaults to "\%Y-\%m-\%d" }
  \item{cex.axis}{ The magnification to be used for axis annotation relative to the current setting of 'cex', same as in \code{\link{plot}}.  }
  \item{cex.legend}{ The magnification to be used for sizing the legend relative to the current setting of 'cex'.  }
  \item{cex.labels}{ The magnification to be used for event line labels relative to the current setting of 'cex'.  }
  \item{\dots}{ any other passthru parameters }
}
\details{
  This function is intended to be used in a wrapper that is written for a particular purpose.
}
\value{
  Draws a timeseries plot of type "line" with some sensible defaults.
}
\references{ }
\author{ Peter Carl }
\note{ }
\seealso{ \code{\link{plot}}, \code{\link{par}} }
\examples{

# These are start and end dates, formatted the same way as the default axis labels
cycles.dates = list(
    c("10/26","11/27"),
    c("08/29","03/33"),
    c("05/37","06/38"),
    c("02/45","10/45"),
    c("11/48","10/49"),
    c("07/53","05/54"),
    c("08/57","04/58"),
    c("04/60","02/61"),
    c("12/69","11/70"),
    c("11/73","03/75"),
    c("01/80","07/80"),
    c("07/81","11/82"),
    c("07/90","03/91"),
    c("03/01","11/01"))
# Event lists - FOR BEST RESULTS, KEEP THESE DATES IN ORDER
risk.dates = c(
    "10/87",
    "02/94",
    "07/97",
    "08/98",
    "10/98",
    "07/00",
    "09/01")
risk.labels = c(
    "Black Monday",
    "Bond Crash",
    "Asian Crisis",
    "Russian Crisis",
    "LTCM",
    "Tech Bubble",
    "Sept 11")
data(edhec)

R=edhec[,"Funds.of.Funds",drop=FALSE]
Return.cumulative = cumprod.column(1+R) - 1
chart.TimeSeries(Return.cumulative)
chart.TimeSeries(Return.cumulative, colorset = "darkblue", legend.loc = "bottomright", period.areas = cycles.dates, period.color = "lightblue", event.lines = risk.dates, event.labels = risk.labels, event.color = "red", lwd = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
