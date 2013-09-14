\name{EmaxDDGBM}
\alias{EmaxDDGBM}
\title{Summary of Expected Drawdown using Brownian Motion Assumptions and Return-Volatility}
\usage{
  EmaxDDGBM(R, digits = 4)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{digits}{significant number}
}
\description{
  Works on the model specified by Maddon-Ismail which
  investigates the behavior of this statistic for a
  Brownian motion with drift.
}
\details{
  If X(t) is a random process on [0, T ], the maximum
  drawdown at time T , D(T), is defined by where \deqn{D(T)
  = sup [X(s) - X(t)]} where s belongs to [0,t] and s
  belongs to [0,T] Informally, this is the largest drop
  from a peak to a bottom. In this paper, we investigate
  the behavior of this statistic for a Brownian motion with
  drift. In particular, we give an infinite series
  representation of its distribution, and consider its
  expected value. When the drift is zero, we give an
  analytic expression for the expected value, and for
  non-zero drift, we give an infinite series
  representation. For all cases, we compute the limiting
  \bold{(\eqn{T tends to \infty})} behavior, which can be
  logarithmic (\eqn{\mu} > 0), square root (\eqn{\mu} = 0),
  or linear (\eqn{\mu} < 0).
}
\examples{
library(PerformanceAnalytics)
data(edhec)
EmaxDDGBM(edhec)
}
\author{
  Shubhankit Mohan
}
\references{
  Magdon-Ismail, M., Atiya, A., Pratap, A., and Yaser S.
  Abu-Mostafa: On the Maximum Drawdown of a Browninan
  Motion, Journal of Applied Probability 41, pp. 147-161,
  2004
  \url{http://alumnus.caltech.edu/~amir/drawdown-jrnl.pdf}
}
\keyword{Assumptions}
\keyword{Brownian}
\keyword{Drawdown}
\keyword{Expected}
\keyword{Motion}
\keyword{Using}
\keyword{models}

