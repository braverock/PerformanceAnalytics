\name{Return.Okunev}
\alias{Return.Okunev}
\title{OW Return Model}
\usage{
  Return.Okunev(R, q = 3)
}
\description{
  The objective is to determine the true underlying return
  by removing the autocorrelation structure in the original
  return series without making any assumptions regarding
  the actual time series properties of the underlying
  process. We are implicitly assuming by this approach that
  the autocorrelations that arise in reported returns are
  entirely due to the smoothing behavior funds engage in
  when reporting results. In fact, the method may be
  adopted to produce any desired level of autocorrelation
  at any lag and is not limited to simply eliminating all
  autocorrelations.It can be be said as the general form of
  Geltner Return Model
}
\details{
  dffd
}
\examples{
data(managers)
head(Return.Okunev(managers[,1:3]),n=3)
}
\references{
  "Hedge Fund Risk Factors and Value at Risk of Credit
  Trading Strategies , John Okunev & Derek White
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

