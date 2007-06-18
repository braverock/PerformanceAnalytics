\name{chart.BarVaR}
\alias{chart.BarVaR}
\title{ Periodic returns in a bar chart with risk metric overlay }
\description{
  Plots the periodic returns in a bar chart overlayed with a risk metric calculation.
}
\usage{
chart.BarVaR(R, width = 0, gap = 1, risk.line = TRUE, method = c("ModifiedVaR","VaR","StdDev"),
             reference.grid = TRUE, xaxis = TRUE, main = "Title", ylab="Value", xlab="Date",
             date.format = "\%m/\%y", xlim = NA, ylim = NA, lwd = 1, colorset =(1:12),
             p=.99, lty = "13", all = FALSE, ...)
}

\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{width}{ periods specified for rolling-period calculations.  Note that VaR and Std Dev with width=0 are calculated from the start of the timeseries }
  \item{method}{ can be specified as \code{StdDev}, \code{VaR}, or \code{ModifiedVaR} }
  \item{p}{ confidence level for \code{VaR} or \code{ModifiedVaR} calculation, default is .99 }
  \item{risk.line}{ if risk.line is set to \code{FALSE}, then this function just draws the underlying bar chart }
  \item{gap}{ numeric number of periods from start of series to use to train risk calculation }
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
   \code{\link{VaR.CornishFisher}}
}
\examples{
data(edhec)
chart.BarVaR(edhec[,"Funds.of.Funds",drop=FALSE], main="Monthly Returns")
chart.BarVaR(edhec[,"Funds.of.Funds",drop=FALSE], main="Monthly Returns", method="VaR")
chart.BarVaR(edhec[,"Funds.of.Funds",drop=FALSE], main="Monthly Returns", method="StdDev", n=36)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
