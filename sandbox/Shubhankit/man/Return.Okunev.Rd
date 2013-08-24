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
  Given a sample of historical returns \eqn{R(1),R(2), . .
  .,R(T)},the method assumes the fund manager smooths
  returns in the following manner: \deqn{ r(0,t) = \sum
  \beta (i) r(0,t-i) + (1- \alpha)r(m,t) } Where :\deqn{
  \sum \beta (i) = (1- \alpha) } \bold{r(0,t)} : is the
  observed (reported) return at time t (with 0 adjustments
  to reported returns), \bold{r(m,t)} : is the true
  underlying (unreported) return at time t (determined by
  making m adjustments to reported returns).

  To remove the \bold{first m orders} of autocorrelation
  from a given return series we would proceed in a manner
  very similar to that detailed in \bold{
  \code{\link{Return.Geltner}} \cr}. We would initially
  remove the first order autocorrelation, then proceed to
  eliminate the second order autocorrelation through the
  iteration process. In general, to remove any order, m,
  autocorrelations from a given return series we would make
  the following transformation to returns: autocorrelation
  structure in the original return series without making
  any assumptions regarding the actual time series
  properties of the underlying process. We are implicitly
  assuming by this approach that the autocorrelations that
  arise in reported returns are entirely due to the
  smoothing behavior funds engage in when reporting
  results. In fact, the method may be adopted to produce
  any desired level of autocorrelation at any lag and is
  not limited to simply eliminating all autocorrelations.
}
\examples{
data(managers)
head(Return.Okunev(managers[,1:3]),n=3)
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  Okunev, John and White, Derek R., \emph{ Hedge Fund Risk
  Factors and Value at Risk of Credit Trading Strategies}
  (October 2003). Available at SSRN:
  \url{http://ssrn.com/abstract=460641}
}
\seealso{
  \code{\link{Return.Geltner}} \cr
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

