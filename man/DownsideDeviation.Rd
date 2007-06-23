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

To calculate it, we take the subset of returns that are less than the target (or Minimum Acceptable Returns (MAR)) returns and take the differences of those to the target.  We sum the squares and divide by the total number of returns to get a below-target semi-variance.

\deqn{
DownsideDeviation(R , MAR)= \delta_{MAR} = \sqrt{ \frac{\sum^{n}_{t=1}(R_{t} - MAR)^{2}}{n} }
}

This is also useful for calculating semi-deviation or semivariance by setting

MAR = mean(x)

Sortino recommends calculating downside deviation utilizing a continuous fitted distribution rather than the discrete distribution of observations.  This would have significant utility, especially in cases of a small number of observations. He recommends using a lognormal distribution, or a fitted distribution based on a relevant style index, to construct the returns below the MAR to increase the confidence in the final result.  Hopefully, in the future, we'll add a fitted option to this function, and would be happy to accept a contribution of this nature.

}
\value{}
\references{
Sortino, F. and Price, L. Performance Measurement in a Downside Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65. \cr

Plantinga, A., van der Meer, R. and Sortino, F. The Impact of Downside Risk on Risk-Adjusted Performance of Mutual Funds in the Euronext Markets. July 19, 2001. Available at SSRN: \url{http://ssrn.com/abstract=277352}  \cr

\url{http://www.sortino.com/htm/performance.htm} see especially end note 10

}
\author{ Peter Carl }
\note{}
\seealso{ }
\examples{
data(edhec)
head(edhec[,6,drop=FALSE])
sd(edhec[,6])
DownsideDeviation(edhec[,6])  # MAR 0\%
DownsideDeviation(edhec[,6], MAR = .04/12) #MAR 4\%
DownsideDeviation(edhec[,6], MAR = mean(edhec[,6])) #MAR = mean return, calculate semivariance
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
