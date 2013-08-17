\name{Return.GLM}
\alias{Return.GLM}
\title{GLM Return Model}
\usage{
  Return.GLM(edhec,4)
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
  smoothingand develop estimators for the smoothing proﬁle
  as well as a smoothing-adjusted Sharpe ratio.
}
\details{
  To quantify the impact of all of these possible sources
  of serial correlation, denote by R(t) the true economic
  return of a hedge fund in period 't'; and let R(t)
  satisfy the following linear single-factor model: where:
  \deqn{R(0,t) = \theta_{0}R(t) + \theta_{1}R(t-1) +
  \theta_{2}R(t-2) ....  + \theta_{k}R(t-k)} where
  \eqn{\theta}'i is defined as the weighted lag of
  autocorrelated lag and whose sum is 1.
}
\author{
  Brian Peterson,Peter Carl, Shubhankit Mohan
}
\references{
  Mila Getmansky, Andrew W. Lo, Igor Makarov,\emph{An
  econometric model of serial correlation and and
  illiquidity in hedge fund Returns},Journal of Financial
  Economics 74 (2004).
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

