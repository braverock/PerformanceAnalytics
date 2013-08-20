\name{table.EMaxDDGBM}
\alias{table.EMaxDDGBM}
\title{Expected Drawdown using Brownian Motion Assumptions}
\usage{
  table.EMaxDDGBM(R, digits = 4)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}
}
\description{
  Works on the model specified by Maddon-Ismail which
  investigates the behavior of this statistic for a
  Brownian motion with drift.
}
\examples{
library(PerformanceAnalytics)
data(edhec)
table.EmaxDDGBM(edhec)
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  An Analysis of the maximum drawdown measure,\emph{Journal
  of Applied Probability} (2004)
}
\keyword{Assumptions}
\keyword{Brownian}
\keyword{Drawdown}
\keyword{Expected}
\keyword{models}
\keyword{Motion}
\keyword{Using}

