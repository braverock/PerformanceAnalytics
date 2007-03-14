\name{chart.QQPlot}
\alias{chart.QQPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper for qq.plot, with sensible defaults }
\description{
A wrapper to create a QQPlot chart of relative returns through time
}
\usage{
chart.QQPlot(R, colorset = (1:12), symbolset = 1, xlab = NULL, ylab = NULL, main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{symbolset}{ from \code{pch} in \code{\link{plot}},  submit a set of symbols to be used in the same order as the data sets submitted }
  \item{xlab}{ set the x-axis label, as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, as in \code{\link{plot}} }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{\dots}{ any other passthru parameters }
}
\value{
QQ Plot
}
\references{ }
\author{ Peter Carl }
\seealso{ }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
