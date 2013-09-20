\name{table.normDD}
\alias{table.normDD}
\title{Generalised Lambda Distribution  Drawdown}
\usage{
  table.normDD(R, digits = 4)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{digits}{number of rounding off digits.}
}
\description{
  When selecting a hedge fund manager, one risk measure
  investors often consider is drawdown. How should drawdown
  distributions look? Carr Futures' Galen Burghardt, Ryan
  Duncan and Lianyan Liu share some insights from their
  research to show investors how to begin to answer this
  tricky question
}
\details{
  To simulate net asset value (NAV) series where skewness
  and kurtosis are zero, we draw sample returns from a
  lognormal return distribution. To capture skewness and
  kurtosis, we sample returns from a \bold{generalised
  \eqn{\lambda} distribution}.The values of skewness and
  excess kurtosis used were roughly consistent with the
  range of values the paper observed for commodity trading
  advisers in our database. The NAV series is constructed
  from the return series. The simulated drawdowns are then
  derived and used to produce the theoretical drawdown
  distributions. A typical run usually requires
  \bold{10,000} iterations to produce a smooth
  distribution.
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  Burghardt, G., and L. Liu, \emph{ It's the
  Autocorrelation, Stupid (November 2012) Newedge working
  paper.} \code{\link[stats]{}} \cr
  \url{http://www.amfmblog.com/assets/Newedge-Autocorrelation.pdf}
  Burghardt, G., Duncan, R. and L. Liu, \emph{Deciphering
  drawdown}. Risk magazine, Risk management for investors,
  September, S16-S20, 2003.
  \url{http://www.risk.net/data/risk/pdf/investor/0903_risk.pdf}
}
\seealso{
  Drawdowns.R
}
\keyword{Assumptions}
\keyword{Brownian}
\keyword{Drawdown}
\keyword{Motion}
\keyword{Simulated}
\keyword{Using}

