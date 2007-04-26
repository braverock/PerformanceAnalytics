\name{table.Drawdowns}
\alias{table.Drawdowns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Worst Drawdowns Summary: Statistics and Stylized Facts }
\description{
Creates table showing statistics for the worst drawdowns.
}
\usage{
table.Drawdowns(R, top= 5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{top}{ the number of drawdowns to include }
  \item{\dots}{ any other passthru parameters }
}
\details{
\preformatted{
From        Trough         To       Depth         Length To Trough Recovery
2001-02-28  2002-09-30  2003-08-31 -0.10746342     31        20       11
1998-07-31  1998-08-31  1998-11-30 -0.05576688      5         2        3
2006-05-31  2006-07-31  2006-11-30 -0.03385062      7         3        4
2000-04-30  2000-05-31  2000-06-30 -0.02960503      3         2        1
2004-04-30  2004-08-31  2004-11-30 -0.02839620      8         5        3
}
}
\value{
A data.frame organized as a table of drawdown statistics.
}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link{DownsideDeviation}}  \cr
    \code{\link{maxDrawdown}}   \cr
    \code{\link{findDrawdowns}} \cr
    \code{\link{sortDrawdowns}} \cr
    \code{\link{chart.Drawdown}} \cr
    \code{\link{table.DownsideRisk}}
}
\examples{
data(edhec)
table.Drawdowns(edhec[,"Long.Short.Equity"])
table.Drawdowns(edhec[,"Relative.Value"])

table.Drawdowns(download.SP500PriceReturns(end="2006-12-31"))
# note that this example shows a drawdown that has not yet ended
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
