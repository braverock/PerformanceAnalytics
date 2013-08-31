\name{chart.AcarSim}
\alias{chart.AcarSim}
\title{Acar-Shane Maximum Loss Plot}
\usage{
  chart.AcarSim(R)
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
  varying from minus \emph{two to two} by step of
  \emph{0.1} . The process has been repeated \bold{six
  thousand times}.
}
\details{
  Unfortunately, there is no \bold{analytical formulae} to
  establish the maximum drawdown properties under the
  random walk assumption. We should note first that due to
  its definition, the maximum drawdown divided by
  volatility is an only function of the ratio mean divided
  by volatility. \deqn{MD/[\sigma]= Min (\sum[X(j)])/\sigma
  = F(\mu/\sigma)} Where j varies from 1 to n ,which is the
  number of drawdown's in simulation
}
\examples{
require(PerformanceAnalytics)
 library(PerformanceAnalytics)
 data(edhec)
chart.AcarSim(edhec)
}
\author{
  Shubhankit Mohan
}
\references{
  Maximum Loss and Maximum Drawdown in Financial
  Markets,\emph{International Conference Sponsored by BNP
  and Imperial College on: Forecasting Financial Markets,
  London, United Kingdom, May 1997}
  \url{http://www.intelligenthedgefundinvesting.com/pubs/easj.pdf}
}
\keyword{Drawdown}
\keyword{Loss}
\keyword{Maximum}
\keyword{Simulated}

