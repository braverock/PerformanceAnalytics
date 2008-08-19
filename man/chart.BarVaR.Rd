\name{chart.BarVaR}
\alias{chart.BarVaR}
\title{ Periodic returns in a bar chart with risk metric overlay }
\description{
  Plots the periodic returns in a bar chart overlayed with a risk metric calculation.
}
\usage{
chart.BarVaR(R, width = 0, gap = 12, methods = c("none", "ModifiedVaR","GaussianVaR","HistoricalVaR", "StdDev"), clean = c("none", "boudt"), reference.grid = TRUE, xaxis = TRUE, main = "Title", ylab="Value", xlab="Date", date.format = "\%m/\%y", xlim = NA, ylim = NA, lwd = 1, colorset = 1:12, p=.99, lty = c(1,2,4,5,6), all = FALSE, show.clean = FALSE, show.horizontal = FALSE, show.symmetric = FALSE, legend.loc="bottomleft", ypad=0, legend.cex = 0.8, ...)
}

\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{width}{ periods specified for rolling-period calculations.  Note that VaR and Std Dev with width=0 are calculated from the start of the timeseries }
  \item{gap}{ numeric number of periods from start of series to use to train risk calculation }
  \item{methods}{ any of \emph{StdDev}, \emph{HistoricalVaR}, \emph{GaussianVaR}, or \emph{ModifiedVaR}, defaults to HistoricalVaR and ModifiedVaR }
  \item{p}{ confidence level for \code{VaR} or \code{ModifiedVaR} calculation, default is .99 }
  \item{reference.grid}{ if true, draws a grid aligned with the points on the x and y axes }
  \item{xaxis}{ if true, draws the x axis }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{date.format}{ re-format the dates for the xaxis; the default is "\%m/\%y" }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{lwd}{ set the line width, same as in \code{\link{plot}} }
  \item{lty}{ set the line type, same as in \code{\link{plot}} }
  \item{all}{ if TRUE, calculates risk lines for each column given in R.  If FALSE, only calculates the risk line for the first column }
  \item{show.clean} { if TRUE and a method for 'clean' is specified, overlays the actual data with the "cleaned" data.  See \code{\link{Return.clean}} for more detail}
  \item{clean} { the method to use to clean outliers from return data prior to risk metric estimation. See \code{\link{Return.clean}} and \code{\link{VaR.CornishFisher}} for more detail }
  \item{show.horizontal} { if TRUE, shows a line across the timeseries at the value of the most recent VaR estimate, to help the reader evaluate the number of exceptions thus far }
  \item{show.symmetric} { if TRUE and the metric is symmetric, this will show the metric's positive values as well as negative values, such as for method "StdDev".}
  \item{legend.loc} { legend location, such as in \code{\link{chart.TimeSeries}}}
  \item{ypad} { adds a numerical padding to the y-axis to keep the data away when legend.loc="bottom".  See examples below.}
  \item{legend.cex} { sets the legend text size, such as in \code{\link{chart.TimeSeries}}}
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{\dots}{ any other passthru parameters }
}
\details{
  Note that \code{StdDev} and \code{VaR} are symmetric calculations, so a high and low measure will be plotted.  \code{ModifiedVaR}, on the other hand, is assymetric and only a lower bound will be drawn.
}
\value{
Creates a plot of time on the x-axis and vertical lines for each period to indicate value on the y-axis.  Overlays a line to indicate the value of a risk metric calculated at that time period.
}
\references{ }
\author{ Peter Carl }
\note{
}
\seealso{
   \code{\link{chart.TimeSeries}} \cr
   \code{\link{plot}} \cr
   \code{\link[fPortfolio]{VaR}} \cr
   \code{\link{VaR.CornishFisher}} \cr
   \code{\link{Return.clean}}
}
\examples{
data(managers)
# plain
chart.BarVaR(managers[,1,drop=FALSE], main="Monthly Returns")

# with risk line
chart.BarVaR(managers[,1,drop=FALSE], methods="HistoricalVaR", main="... with Empirical VaR from Inception")

# with lines for all managers in the sample
chart.BarVaR(managers[,1:6], methods="GaussianVaR", all=TRUE, lty=1, lwd=2, colorset= c("red", rep("gray", 5)), main="... with Gaussian VaR and Estimates for Peers")

# with multiple methods
chart.BarVaR(managers[,1,drop=FALSE],methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"), main="... with Multiple Methods")

# cleaned up a bit
chart.BarVaR(managers[,1,drop=FALSE],methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"), lwd=2, ypad=.01, main="... with Padding for Bottom Legend")

# with 'cleaned' data for VaR estimates
chart.BarVaR(managers[,1,drop=FALSE],methods=c("HistoricalVaR", "ModifiedVaR"), lwd=2, ypad=.01, clean="boudt", main="... with Robust ModVaR Estimate")

# Cornish Fisher VaR estimated with cleaned data, with horizontal line to show exceptions
chart.BarVaR(managers[,1,drop=FALSE],methods="ModifiedVaR", lwd=2, ypad=.01, clean="boudt", show.horizontal=TRUE, lty=2, main="... with Robust ModVaR and Line for Identifying Exceptions")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
