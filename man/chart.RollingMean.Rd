\name{chart.RollingMean}
\alias{chart.RollingMean}
\title{chart the rolling mean return}
\usage{
  chart.RollingMean(R, width = 12, xaxis = TRUE,
    ylim = NULL, lwd = c(2, 1, 1), ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{width}{number of periods to apply rolling function
  window over}

  \item{xaxis}{if true, draws the x axis}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}}

  \item{na.pad}{TRUE/FALSE If TRUE it adds any times that
  would not otherwise have been in the result with a value
  of NA. If FALSE those times are dropped.}

  \item{lwd}{set the line width, same as in
  \code{\link{plot}}.  Specified in order of the main line
  and the two confidence bands.}

  \item{\dots}{any other passthru parameters}
}
\description{
  A wrapper to create a rolling mean return chart with 95%
  confidence bands.
}
\examples{
data(edhec)
chart.RollingMean(edhec[, 9, drop = FALSE])
}
\author{
  Peter Carl
}
\keyword{distribution}
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

