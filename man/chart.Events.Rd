\name{chart.Events}
\Rdversion{1.1}
\alias{chart.Events}
\title{ Plots a time series with event dates aligned }
\description{
Creates a time series plot where events given by a set of dates are aligned, with the adjacent prior and posterior time series data plotted in order.  The x-axis is time, but relative to the date specified, e.g., number of months preceeding or following the events.   
}
\usage{
chart.Events(R, dates, prior = 12, post = 12, main = NULL, xlab = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{dates}{ a list of dates (e.g., \code{c("09/03","05/06"))} formatted the same as in R.  This function matches the re-formatted row or index names (dates) with the given list, so to get a match the formatting needs to be correct. }
  \item{prior}{ the number of periods to plot prior to the event.  Interpreted as a positive number.}
  \item{post}{ the number of periods to plot following to the event.  Interpreted as a positive number. }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{\dots}{  any other passthru parameters to the \code{\link{plot}} function }
}
\details{
This is a chart that is commonly used for event studies in econometrics, usually with recession dates, to demonstrate the path of a time series going into and coming out of an event.  The time axis is simply the number of periods prior and following the event, and each line represents a different event.  Note that if time periods are close enough together and the window shown is wide enough, the data will appear to repeat.  That can be confusing, but the function does not currently allow for different windows around each event.
}
\value{ Draws a timeseries plot of type "line" on a numbered x-axis, with a vertical dashed line indicating the alignment of the dates given. }
\references{
%% ~put references to the literature/web site here ~
}
\author{ Peter Carl }

\seealso{
\code{\link{chart.TimeSeries}}, \code{\link{plot}}, \code{\link{par}}
}
\examples{
data(managers)
#   time(managers) = as.Date(time(managers))
#   time(R) = as.Date(time(R))
R = Drawdowns(managers[,2,drop=F])
n = table.Drawdowns(managers[,2,drop=F])                          
chart.Events(Drawdowns(managers[,2,drop=F]), dates = n$Trough, prior=max(na.omit(n$"To Trough")), post=max(na.omit(n$Recovery)), lwd=2, colorset=redfocus, legend.loc=NULL, main = "Worst Drawdowns")
}
\keyword{ ts }
\keyword{ hplot }
