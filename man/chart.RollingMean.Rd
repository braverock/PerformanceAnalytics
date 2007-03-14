\name{chart.RollingMean}
\alias{chart.RollingMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ chart the rolling mean return }
\description{
A wrapper to create a rolling mean return chart with 95\% confidence bands.
}
\usage{
chart.RollingMean(R, n = 12, xaxis = TRUE, ylim = NULL, main = paste("Rolling ", n, "-Month Mean Return", sep = ""), trim = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{n}{ number of periods to apply rolling function over }
  \item{xaxis}{ if true, draws the x axis }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{\dots}{ any other passthru parameters }
}
\value{
A timeseries line charts of the rolling mean, with error bars
}
\references{ }
\author{ Peter Carl }
\seealso{}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }