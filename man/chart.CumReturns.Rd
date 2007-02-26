\name{chart.CumReturns}
\alias{chart.CumReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Cumulates and graphs a set of periodic returns  }
\description{
  Chart that cumulates the periodic returns given and draws a line graph of the results as a "wealth index".
}
\usage{
chart.CumReturns(R, wealth.index = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{wealth.index}{ if \code{wealth.index} is \code{TRUE}, shows the "value of \$1", starting the cumulation of returns at 1 rather than zero }
  \item{\dots}{ any other passthru parameters }
}
\details{

}
\value{
A timeseries line chart of the cumulative return series
}
\references{ }
\author{ Peter Carl }
\note{
}
\seealso{ See Also \code{\link{chart.TimeSeries}}, \code{\link{plot}} }
\examples{
data(edhec)
chart.CumReturns(edhec[,"Funds.of.Funds"],main="Cumulative Returns")
chart.CumReturns(edhec[,"Funds.of.Funds"],wealth.index=TRUE, main="Value of \$1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
