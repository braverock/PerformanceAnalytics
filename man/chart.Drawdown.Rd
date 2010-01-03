\name{chart.Drawdown}
\alias{chart.Drawdown}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Time series chart of drawdowns through time }
\description{
  A time series chart demonstrating drawdowns from peak equity attained through time, calculated from periodic returns.
}
\usage{
chart.Drawdown(R, geometric=FALSE, legend.loc = NULL, colorset = (1:12), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default FALSE }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{\dots}{ any other passthru parameters }
}
\details{
Any time the cumulative returns dips below the maximum cumulative returns, it's a drawdown.  Drawdowns are measured as a percentage of that maximum cumulative return, in effect, measured from peak equity.
}

\author{ Peter Carl }
\seealso{

    \code{\link{plot}} \cr
    \code{\link{chart.TimeSeries}} \cr
    \code{\link{findDrawdowns}} \cr
    \code{\link{sortDrawdowns}} \cr
    \code{\link{maxDrawdown}} \cr
    \code{\link{table.Drawdowns}} \cr
    \code{\link{table.DownsideRisk}}

}
\examples{
data(edhec)
chart.Drawdown(edhec[,c(1,2)], main="Drawdown from Peak Equity Attained", legend.loc="bottomleft")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
