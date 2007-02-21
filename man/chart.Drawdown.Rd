\name{chart.Drawdown}
\alias{chart.Drawdown}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Time series chart of drawdowns through time }
\description{
  A time series chart demonstrating drawdowns from peak equity attained through time, calculated from periodic returns.
}
\usage{
chart.Drawdown(R, legend.loc = NULL, colorset = (1:12), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{\dots}{ any other passthru parameters }
}
\details{
Any time the cumulative returns dips below the maximum cumulative returns, it's a drawdown.  Drawdowns are measured as a percentage of that maximum cumulative return, in effect, measured from peak equity.
}
\value{
  A chart with time on the x-axis and the value peak equity attained on the y-axis.
}
\references{ }
\author{ Peter Carl }
\note{ 
}
\seealso{ \code{\link{plot}}, \code{\link{chart.TimeSeries}}  }
\examples{
data(edhec)
chart.Drawdown(edhec[,c(1,2),drop=F], main="Drawdown from Peak Equity Attained", legend.loc="bottomleft")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ chart }
\keyword{ timeseries }
\keyword{ drawdown }
\keyword{ plot }
