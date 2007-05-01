\name{chart.CumReturns}
\alias{chart.CumReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Cumulates and graphs a set of periodic returns  }
\description{
  Chart that cumulates the periodic returns given and draws a line graph of the results as a "wealth index".
}
\usage{
function(R, wealth.index = FALSE, legend.loc = NULL, colorset =(1:12), begin = c("axis", "first"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{wealth.index}{ if \code{wealth.index} is \code{TRUE}, shows the "value of \$1", starting the cumulation of returns at 1 rather than zero }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{begin}{ sets the beginning of shorter timeseries to either start at the axis, or start at the first (usually a reference) series given }
  \item{\dots}{ any other passthru parameters }
}
\details{
Cumulates the return series and displays either as a wealth index or as cumulative returns.
}
\value{
A timeseries line chart of the cumulative return series.
}
\references{ }
\author{ Peter Carl }
\note{
}
\seealso{
    \code{\link{chart.TimeSeries}} \cr
    \code{\link{plot}}
}
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
