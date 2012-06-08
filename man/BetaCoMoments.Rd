\name{BetaCoMoments}
\alias{BetaCoKurtosis}
\alias{BetaCoMoments}
\alias{BetaCoSkewness}
\alias{BetaCoVariance}
\alias{SystematicKurtosis}
\alias{SystematicSkewness}
\title{Functions to calculate systematic or beta co-moments of return series}
\usage{
  BetaCoVariance(Ra, Rb)

  BetaCoSkewness(Ra, Rb, test = FALSE)

  BetaCoKurtosis(Ra, Rb)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rb}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of index, benchmark, or secondary asset
  returns to compare against}

  \item{test}{condition not implemented yet}
}
\description{
  Functions to calculate systematic or beta co-moments of
  return series
}
\examples{
data(managers)

BetaCoVariance(managers[, "HAM2", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
BetaCoSkewness(managers[, "HAM2", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
BetaCoKurtosis(managers[, "HAM2", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
BetaCoKurtosis(managers[,1:6], managers[,8,drop=FALSE])
BetaCoKurtosis(managers[,1:6], managers[,8:7])
}
\author{
  Kris Boudt, Peter Carl, Brian Peterson
}
\references{
  Boudt, Kris, Brian G. Peterson, and Christophe Croux.
  2008. Estimation and Decomposition of Downside Risk for
  Portfolios with Non-Normal Returns. Journal of Risk.
  Winter.

  Martellini, Lionel, and Volker Ziemann. 2007. Improved
  Forecasts of Higher-Order Comoments and Implications for
  Portfolio Selection. EDHEC Risk and Asset Management
  Research Centre working paper.
}
\seealso{
  \code{\link{CoMoments}}
}
\concept{
  beta co-moments

  moments
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

