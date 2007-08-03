\name{DownsideDeviation}
\alias{DownsideDeviation}
\alias{SemiDeviation}
\alias{SemiVariance}
\title{ downside risk (deviation, variance) of the return distribution }

\description{
Downside deviation, semideviation, and semivariance are measures of downside risk.
}

\usage{
DownsideDeviation(Ra, MAR = 0, method=c("full","subset"))
SemiDeviation(Ra)
SemiVariance(Ra)
}

\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{MAR}{ Minimum Acceptable Return, in the same periodicity as your returns }
  \item{method}{ one of "full" or "subset", indicating whether to use the length of the full series or the length of the subset of the series below the MAR as the denominator}
}
\details{
Downside deviation, similar to semi deviation, eliminates positive returns when calculating risk.  Instead of using the mean return or zero, it uses the Minimum Acceptable Return as proposed by Sharpe (which may be the mean historical return or zero).

To calculate it, we take the subset of returns that are less than the target (or Minimum Acceptable Returns (MAR)) returns and take the differences of those to the target.  We sum the squares and divide by the total number of returns to get a below-target semi-variance.

\deqn{
DownsideDeviation(R , MAR)= \delta_{MAR} = \sqrt{ \frac{\sum^{n}_{t=1}(R_{t} - MAR)^{2}}{n} }
}

where \eqn{n} is either the number of observations of the entire series or the number of observations in the subset of the series falling below the MAR.

SemiDeviation or SemiVariance is a popular alternative downside risk measure that may be used in place of standard deviation or variance. SemiDeviation and SemiVariance are implemented as a wrapper of DownsideDeviation with MAR=mean(R).

In many functions like Markowitz optimization, semideviation may be substituted directly, and the covariance matrix may be constructed from semideviation or the vector of returns below the mean rather than from variance or the full vector of returns.

In semideviation, by convention, the value of \eqn{n} is set to the full number of pobservations. In semivariance the the value of \eqn{n} is set to the subset of returns below the mean.  It should be noted that while this is the correct mathematical definition of semivariance, this result doesn't make any sense if you are also going to be using the time series of returns below the mean or below a MAR to construct a semi-covariance matrix for portfolio optimization.

Sortino recommends calculating downside deviation utilizing a continuous fitted distribution rather than the discrete distribution of observations.  This would have significant utility, especially in cases of a small number of observations. He recommends using a lognormal distribution, or a fitted distribution based on a relevant style index, to construct the returns below the MAR to increase the confidence in the final result.  Hopefully, in the future, we'll add a fitted option to this function, and would be happy to accept a contribution of this nature.

}
\value{float value of the calculation}
\references{
Sortino, F. and Price, L. Performance Measurement in a Downside Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65. \cr

Plantinga, A., van der Meer, R. and Sortino, F. The Impact of Downside Risk on Risk-Adjusted Performance of Mutual Funds in the Euronext Markets. July 19, 2001. Available at SSRN: \url{http://ssrn.com/abstract=277352}  \cr

\url{http://www.sortino.com/htm/performance.htm} see especially end note 10

\url{http://en.wikipedia.org/wiki/Semivariance}

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

SemiDeviation(edhec[,7])
SemiVariance (edhec[,7]) #calculated using method="subset"
}

\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }