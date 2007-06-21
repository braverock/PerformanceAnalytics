\name{DownsideDeviation}
\alias{DownsideDeviation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ function for downside risk of the return distribution }
\description{
Downside deviation is an alternative measure of dounside risk.
}
\usage{
DownsideDeviation(Ra, MAR = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{MAR}{ Minimum Acceptable Return, in the same periodicity as your returns }
}
\details{
Downside deviation, similar to semi deviation, eliminates positive returns when calculating risk.  Instead of using the mean return or zero, it uses the Minimum Acceptable Return as proposed by Sharpe (which may be the mean historical return or zero).

To calculate it, we take the returns that are less than the target (or Minimum Acceptable Returns (MAR)) returns and take the differences of those to the target.  We sum the squares and divide by the total number of returns to get a below-target semi-variance.

This is also useful for calculating semi-deviation or semivariance by setting

MAR = mean(x)
}
\value{}
\references{
Sortino, F. and Price, L. Performance Measurement in a Downside Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65. \cr

Plantinga, A., van der Meer, R. and Sortino, F. The Impact of Downside Risk on Risk-Adjusted Performance of Mutual Funds in the Euronext Markets. July 19, 2001. Available at SSRN: \url{http://ssrn.com/abstract=277352}

}
\author{ Peter Carl }
\note{}
\seealso{ }
\examples{
data(edhec)
head(edhec[,6,drop=FALSE])
sd(edhec[,6])
DownsideDeviation(edhec[,6])
DownsideDeviation(edhec[,6], MAR = .04/12)
DownsideDeviation(edhec[,6], MAR = mean(edhec[,6]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
