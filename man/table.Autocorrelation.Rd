\name{table.Autocorrelation}
\alias{table.Autocorrelation}
\title{table for calculating the first six autocorrelation coefficients and
significance}
\usage{
table.Autocorrelation(R, digits = 4)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{digits}{number of digits to round results to for
  display}
}
\description{
Produces data table of autocorrelation coefficients
\eqn{\rho}{rho} and corresponding Q(6)-statistic for each
column in R.
}
\note{
To test returns for autocorrelation, Lo (2001) suggests the
use of the Ljung-Box test, a significance test for the
auto-correlation coefficients. Ljung and Box (1978) provide
a refinement of the Q-statistic proposed by Box and Pierce
(1970) that offers a better fit for the \eqn{\chi^2}{chi^2}
test for small sample sizes. \code{\link{Box.test}}
provides both.
}
\examples{
data(managers)
t(table.Autocorrelation(managers))

result = t(table.Autocorrelation(managers[,1:8]))
textplot(result, rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=15, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
title(main="Autocorrelation")
}
\author{
Peter Carl
}
\references{
Lo, Andrew W. 2001. Risk Management for Hedge Funds:
Introduction and Overview. SSRN eLibrary.
}
\seealso{
\code{\link{Box.test}}, \code{\link{acf}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

