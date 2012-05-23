\name{chart.RollingPerformance}
\alias{chart.RollingPerformance}
\title{wrapper to create a chart of rolling performance metrics in a line chart}
\usage{
  chart.RollingPerformance(R, width = 12,
    FUN = "Return.annualized", ..., na.pad = TRUE,
    ylim = NULL, main = NULL)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{width}{number of periods to apply rolling function
  window over}

  \item{FUN}{any function that can be evaluated using a
  single set of returns (e.g., rolling
  \code{\link{CAPM.beta}} won't work, but
  \code{\link{Return.annualized}} will)}

  \item{na.pad}{TRUE/FALSE If TRUE it adds any times that
  would not otherwise have been in the result with a value
  of NA. If FALSE those times are dropped.}

  \item{main}{set the chart title, same as in
  \code{\link{plot}}}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}}

  \item{\dots}{any other passthru parameters to
  \code{\link{plot}} or the function specified}
}
\description{
  A wrapper to create a chart of rolling performance
  metrics in a line chart
}
\examples{
data(edhec)
chart.RollingPerformance(edhec[, 1:3], width = 24)
chart.RollingPerformance(edhec[, 1:3],
		FUN = 'mean', width = 24, colorset = rich8equal,
		lwd = 2, legend.loc = "topleft",
		main = "Rolling 24-Month Mean Return")
chart.RollingPerformance(edhec[, 1:3],
		FUN = 'SharpeRatio.annualized', width = 24,
		colorset = rich8equal, lwd = 2, legend.loc = "topleft",
		main = "Rolling 24-Month Sharpe Ratio")
}
\author{
  Peter Carl
}
\seealso{
  \code{\link{charts.RollingPerformance}},
  \code{\link[zoo]{rollapply}}
}
\keyword{distribution}
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

