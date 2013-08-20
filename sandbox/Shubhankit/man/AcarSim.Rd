\name{AcarSim}
\alias{AcarSim}
\title{Acar and Shane Maximum Loss}
\usage{
  AcarSim(R)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}
}
\description{
  To get some insight on the relationships between maximum
  drawdown per unit of volatility and mean return divided
  by volatility, we have proceeded to Monte-Carlo
  simulations. We have simulated cash flows over a period
  of 36 monthly returns and measured maximum drawdown for
  varied levels of annualised return divided by volatility
  varying from minus two to two by step of 0.1. The process
  has been repeated six thousand times.
}
\examples{
library(PerformanceAnalytics)
AcarSim(edhec)
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  Maximum Loss and Maximum Drawdown in Financial
  Markets,\emph{International Conference Sponsored by BNP
  and Imperial College on: Forecasting Financial Markets,
  London, United Kingdom, May 1997}
}
\keyword{Drawdown}
\keyword{Loss}
\keyword{Maximum}
\keyword{Simulared}

