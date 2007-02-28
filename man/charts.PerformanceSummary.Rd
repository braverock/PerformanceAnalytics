\name{charts.PerformanceSummary}
\alias{charts.PerformanceSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create combined wealth index, period performance, and drawdown chart }
\description{
For a set of returns, create a wealth index chart, bars for monthly performance, and underwater chart for drawdown.
}
\usage{
charts.PerformanceSummary(R, rf = 0, main = NULL, method = "ModifiedVaR", n = 0, event.labels = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{method}{ Used to select the risk parameter to use in the chart.BarVaR.
    May be any of: \cr
    modVaR - uses CF modified VaR \cr
    VaR - uses traditional Value at Risk \cr
    StdDev - monthly standard deviation of trailing 12 month returns
  }
  \item{n}{ number of periods to apply rolling function over }
  \item{event.labels}{ TRUE/FALSE whether or not to display lines and labels for historical market shock events }
  \item{\dots}{ any other passthru parameters }
}
\details{
}
\value{
A stack of three related timeseries charts.
}
\references{ }
\author{ Peter Carl }
\note{
Most inputs are the same as "\code{\link{plot}}" and are principally included so that some sensible defaults could be set.
}
\seealso{
    \code{\link{chart.CumReturns}}
    \code{\link{chart.BarVaR}}
    \code{\link{chart.Drawdown}}
}
\examples{
data(edhec)
charts.PerformanceSummary(edhec[,c("Funds.of.Funds","Long.Short.Equity")])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
