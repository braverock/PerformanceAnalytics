\name{table.NormDD}
\alias{table.NormDD}
\title{To simulate net asset value (NAV) series where skewness and kurtosis are zero,
we draw sample returns from a lognormal return distribution. To capture skewness
and kurtosis, we sample returns from a generalised lambda distribution.The values of
skewness and excess kurtosis used were roughly consistent with the range of values we
observed for commodity trading advisers in our database. The NAV series is constructed
from the return series. The simulated drawdowns are then derived and used to produce
the theoretical drawdown distributions. A typical run usually requires 10,000
iterations to produce a smooth distribution.}
\usage{
  table.NormDD(R, digits = 4)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}
}
\description{
  To simulate net asset value (NAV) series where skewness
  and kurtosis are zero, we draw sample returns from a
  lognormal return distribution. To capture skewness and
  kurtosis, we sample returns from a generalised lambda
  distribution.The values of skewness and excess kurtosis
  used were roughly consistent with the range of values we
  observed for commodity trading advisers in our database.
  The NAV series is constructed from the return series. The
  simulated drawdowns are then derived and used to produce
  the theoretical drawdown distributions. A typical run
  usually requires 10,000 iterations to produce a smooth
  distribution.
}
\author{
  R
}
\keyword{Assumptions}
\keyword{Brownian}
\keyword{Drawdown}
\keyword{Expected}
\keyword{Motion}
\keyword{Using}

