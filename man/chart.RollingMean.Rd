\name{chart.RollingMean}
\alias{chart.RollingMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ chart the rolling mean return }
\description{
A wrapper to create a rolling mean return chart with 95\% confidence bands.
}
\usage{
chart.RollingMean(R, width = 12, xaxis = TRUE, ylim = NULL, na.pad =
FALSE, lwd = c(2, 1, 1), ...) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{width}{ number of periods to apply rolling function window over }
  \item{xaxis}{ if true, draws the x axis }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{na.pad}{ TRUE/FALSE If TRUE it adds any times that would not otherwise have been in the result with a value of NA. If FALSE those times are dropped. }
  \item{lwd}{ set the line width, same as in \code{\link{plot}}.  Specified in order of the main line and the two confidence bands. }
  \item{\dots}{ any other passthru parameters }
}
\value{
A timeseries line charts of the rolling mean, with error bars
}
\references{ }
\author{ Peter Carl }
\examples{
data(edhec)
chart.RollingMean(edhec[, 9, drop = FALSE])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }