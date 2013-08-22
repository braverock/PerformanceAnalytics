\name{table.SFM}
\alias{table.CAPM}
\alias{table.SFM}
\title{Single Factor Asset-Pricing Model Summary: Statistics and Stylized Facts}
\usage{
  table.SFM(Ra, Rb, scale = NA, Rf = 0, digits = 4)
}
\arguments{
  \item{Ra}{a vector of returns to test, e.g., the asset to
  be examined}

  \item{Rb}{a matrix, data.frame, or timeSeries of
  benchmark(s) to test the asset against.}

  \item{scale}{number of periods in a year (daily scale =
  252, monthly scale = 12, quarterly scale = 4)}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{digits}{number of digits to round results to}
}
\description{
  Takes a set of returns and relates them to a benchmark
  return. Provides a set of measures related to an excess
  return single factor model, or CAPM.
}
\details{
  This table will show statistics pertaining to an asset
  against a set of benchmarks, or statistics for a set of
  assets against a benchmark.
}
\examples{
data(managers)
table.SFM(managers[,1:3,drop=FALSE], managers[,8,drop=FALSE], Rf = managers[,10,drop=FALSE])

result = table.SFM(managers[,1:3,drop=FALSE], managers[,8,drop=FALSE], Rf = managers[,10,drop=FALSE])
textplot(result, rmar = 0.8, cmar = 1.5,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=15, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
title(main="Single Factor Model Related Statistics")
}
\author{
  Peter Carl
}
\seealso{
  \code{\link{CAPM.alpha}} \cr \code{\link{CAPM.beta}} \cr
  \code{\link{TrackingError}} \cr
  \code{\link{ActivePremium}} \cr
  \code{\link{InformationRatio}} \cr
  \code{\link{TreynorRatio}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

