\name{Return.GLM}
\alias{Return.GLM}
\title{GLM Return Model}
\usage{
  Return.GLM(Ra, q = 3)
}
\arguments{
  \item{Ra}{: an xts, vector, matrix, data frame,
  timeSeries or zoo object of asset returns}

  \item{q}{: order of autocorrelation coefficient lag
  factors}
}
\description{
  True returns represent the flow of information that would
  determine the equilibrium value of the fund's securities
  in a frictionless market. However, true economic returns
  are not observed. The returns to hedge funds and other
  alternative investments are often highly serially
  correlated.We propose an econometric model of return
  smoothing and \emph{develop estimators for the smoothing
  profile as well as a smoothing-adjusted Sharpe ratio}.
}
\details{
  To quantify the impact of all of these possible sources
  of serial correlation, denote by R(t) the true economic
  return of a hedge fund in period 't'; and let R(t)
  satisfy the following linear single-factor model: where:
  \deqn{R(0,t) = \theta_{0}R(t) + \theta_{1}R(t-1) +
  \theta_{2}R(t-2) ....  + \theta_{k}R(t-k)} Where :
  \eqn{\theta}'i is defined as the weighted lag of
  autocorrelated lag and whose sum is 1. \deqn{\theta (j)
  \epsilon [0,1] where : j = 0,1,....,k } and, \deqn{\theta
  _1 + \theta _2 + \theta _3 \cdots + \theta _k = 1} Using
  the methods outlined above , the paper estimates the
  smoothing model using maximumlikelihood
  procedure-programmed in Matlab using the Optimization
  Toolbox andreplicated in Stata usingits MA(k) estimation
  routine.Using Time seseries analysis and computational
  finance("\bold{tseries}") library , we fit an it an
  \bold{ARMA} model to a univariate time series by
  conditional least squares. For exact maximum likelihood
  estimation,arima0 from package \bold{stats} can be used.
}
\examples{
library(PerformanceAnalytics)
Return.GLM(edhec,4)
}
\author{
  Brian Peterson,Peter Carl, Shubhankit Mohan
}
\references{
  Mila Getmansky, Andrew W. Lo, Igor Makarov,\emph{An
  econometric model of serial correlation and and
  illiquidity in hedge fund Returns},Journal of Financial
  Economics 74 (2004).\url{
  http://ssrn.com/abstract=384700}
}
\seealso{
  Return.Geltner
}
\keyword{distribution}
\keyword{model}
\keyword{multivariate}
\keyword{ts}

