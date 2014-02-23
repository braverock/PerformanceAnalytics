\name{chart.Correlation}
\alias{chart.Correlation}
\title{correlation matrix chart}
\usage{
chart.Correlation(R, histogram = TRUE, method = c("pearson", "kendall",
  "spearman"), ...)
}
\arguments{
  \item{R}{data for the x axis, can take matrix,vector, or
  timeseries}

  \item{histogram}{TRUE/FALSE whether or not to display a
  histogram}

  \item{method}{a character string indicating which
  correlation coefficient (or covariance) is to be
  computed.  One of "pearson" (default), "kendall", or
  "spearman", can be abbreviated.}

  \item{\dots}{any other passthru parameters into
  \code{\link{pairs}}}
}
\description{
Visualization of a Correlation Matrix. On top the
(absolute) value of the correlation plus the result of the
cor.test as stars. On bottom, the bivariate scatterplots,
with a fitted line
}
\note{
based on plot at
\url{http://addictedtor.free.fr/graphiques/sources/source_137.R}
}
\examples{
data(managers)
chart.Correlation(managers[,1:8], histogram=TRUE, pch="+")
}
\author{
Peter Carl
}
\seealso{
\code{\link{table.Correlation}}
}
\keyword{distribution}
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

