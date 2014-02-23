\name{AdjustedSharpeRatio}
\alias{AdjustedSharpeRatio}
\title{Adjusted Sharpe ratio of the return distribution}
\usage{
AdjustedSharpeRatio(R, Rf = 0, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rf}{the risk free rate}

  \item{\dots}{any other passthru parameters}
}
\description{
Adjusted Sharpe ratio was introduced by Pezier and White
(2006) to adjusts for skewness and kurtosis by
incorporating a penalty factor for negative skewness and
excess kurtosis.
}
\details{
\deqn{Adjusted Sharpe Ratio = SR * [1 + (\frac{S}{6}) * SR
- (\frac{K - 3}{24}) * SR^2]}{Adjusted Sharpe ratio = SR x
[1 + (S/6) x SR - ((K-3) / 24) x SR^2]}

where \eqn{SR} is the sharpe ratio with data annualized,
\eqn{S} is the skewness and \eqn{K} is the kurtosis
}
\examples{
data(portfolio_bacon)
print(AdjustedSharpeRatio(portfolio_bacon[,1])) #expected 0.81

data(managers)
print(AdjustedSharpeRatio(managers['1996']))
print(AdjustedSharpeRatio(managers['1996',1]))
}
\author{
Matthieu Lestel
}
\references{
Carl Bacon, \emph{Practical portfolio performance
measurement and attribution}, second edition 2008 p.99
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

