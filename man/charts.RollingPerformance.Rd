\name{charts.RollingPerformance}
\alias{charts.RollingPerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ rolling performance chart }
\description{
A wrapper to create a rolling annualized returns chart, rolling annualized standard deviation chart, and a rolling annualized sharpe ratio chart.
}
\usage{
charts.RollingPerformance(R, width = 12, Rf = 0, main = NULL, trim = TRUE,
                          event.labels = NULL, legend.loc = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{width}{ number of periods to apply rolling function over }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{event.labels}{ TRUE/FALSE whether or not to display lines and labels for historical market shock events }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{\dots}{ any other passthru parameters }
}
\author{ Peter Carl }
\seealso{
    \code{\link{chart.RollingPerformance}}
}
\examples{
data(managers)
charts.RollingPerformance(managers[,1:8], Rf=managers[,10,drop=FALSE], colorset=tim8equal, main="Rolling 12-Month Performance", legend.loc="topleft")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }