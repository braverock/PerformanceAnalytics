\name{chart.BarVaR}
\alias{chart.BarVaR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Periodic returns in a bar chart with risk metric overlay }
\description{
  Plots the periodic returns in a bar chart overlayed with a risk metric calculation.
}
\usage{
chart.BarVaR(R, n = 0, risk.line = TRUE, method = "ModifiedVaR", reference.grid = TRUE, xaxis = TRUE, main = "Title", ylab="Value", xlab="Date", date.format = "\%m/\%y", xlim = NA, ylim = NA, lwd = 1, colorset =(1:12), p=.99,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{n}{ periods specified for rolling-period calculations.  Note that VaR and Std Dev with n=0 are calculated from the start of the timeseries }
  \item{risk.line}{ if risk.line is set to \code{FALSE}, then this function just draws the underlying bar chart }
  \item{method}{ can be specified as \code{StdDev}, \code{VaR}, or \code{ModifiedVaR} here~~ }
  \item{p}{ confidence level for \code{VaR} or \code{ModifiedVaR} calculation, default is .99 }
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
\seealso{ See Also \code{\link{chart.TimeSeries}}, \code{\link{plot}}, \code{\link{VaR}}, \code{\link{VaR.CornishFisher}} }
\examples{
data(edhec)
chart.BarVaR(edhec[,"Funds.of.Funds",drop=F], main="Monthly Returns")
chart.BarVaR(edhec[,"Funds.of.Funds",drop=F], main="Monthly Returns", method="VaR")
chart.BarVaR(edhec[,"Funds.of.Funds",drop=F], main="Monthly Returns", method="StdDev", n=36)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
