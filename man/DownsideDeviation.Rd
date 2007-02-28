\name{DownsideDeviation}
\alias{DownsideDeviation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ function for downside risk of the return distribution }
\description{
Downside deviation is an alternative measure of dounside risk.
}
\usage{
DownsideDeviation(R, MAR = 0)
SemiDeviation(R)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{MAR}{ Minimum Acceptable Return, in the same periodicity as your returns }
}
\details{
Downside deviation, similar to semi deviation, eliminates positive returns when calculating risk.  Instead of using the mean return or zero, it uses the Minimum Acceptable Return as proposed by Sharpe (which may be the mean historical return or zero).

To calculate it, we take the returns that are less than the target (or Minimum Acceptable Returns (MAR)) returns and take the differences of those to the target.  We sum the squares and divide by the total number of returns to get a below-target semi-variance.

This is also useful for calculating semi-deviation by setting

MAR = mean(x)
}
\value{}
\references{
Sortino, F. and Price, L. Performance Measurement in a Downside Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65.
}
\author{ Peter Carl }
\note{}
\seealso{ }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
