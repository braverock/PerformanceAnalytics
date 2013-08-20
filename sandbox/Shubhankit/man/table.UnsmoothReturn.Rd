\name{table.UnsmoothReturn}
\alias{table.UnsmoothReturn}
\title{Compenent Decomposition of Table of Unsmooth Returns}
\usage{
  table.UnsmoothReturn(R, n = 3, p = 0.95, digits = 4)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{ci}{confidence interval, defaults to 95\%}

  \item{n}{number of series lags}

  \item{p}{confidence level for calculation, default p=.99}

  \item{digits}{number of digits to round results to}
}
\description{
  Creates a table of estimates of moving averages for
  comparison across multiple instruments or funds as well
  as their standard error and smoothing index
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\keyword{models}
\keyword{return}
\keyword{smooth}
\keyword{ts}

