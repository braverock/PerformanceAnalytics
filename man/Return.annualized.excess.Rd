\name{Return.annualized.excess}
\alias{Return.annualized.excess}
\title{calculates an annualized excess return for comparing instruments with different
length history}
\usage{
  Return.annualized.excess(Rp, Rb, scale = NA,
    geometric = TRUE)
}
\arguments{
  \item{Rp}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of portfolio returns}

  \item{Rb}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of benchmark returns}

  \item{scale}{number of periods in a year (daily scale =
  252, monthly scale = 12, quarterly scale = 4)}

  \item{geometric}{generate geometric (TRUE) or simple
  (FALSE) excess returns, default TRUE}
}
\description{
  An average annualized excess return is convenient for
  comparing excess returns.
}
\details{
  Annualized returns are useful for comparing two assets.
  To do so, you must scale your observations to an annual
  scale by raising the compound return to the number of
  periods in a year, and taking the root to the number of
  total observations:
  \deqn{prod(1+R_{a})^{\frac{scale}{n}}-1=\sqrt[n]{prod(1+R_{a})^{scale}}-
  1}{prod(1 + Ra)^(scale/n) - 1}

  where scale is the number of periods in a year, and n is
  the total number of periods for which you have
  observations.

  Finally having annualized returns for portfolio and
  benchmark we can compute annualized excess return as
  difference in the annualized portfolio and benchmark
  returns in the arithmetic case: \deqn{er = R_{pa} -
  R_{ba}}{er = Rpa - Rba}

  and as a geometric difference in the geometric case:
  \deqn{er = \frac{(1 + R_{pa})}{(1 + R_{ba})} - 1}{er = (1
  + Rpa) / (1 + Rba) - 1}
}
\examples{
data(attrib)
Return.annualized.excess(Rp = attrib.returns[, 21], Rb = attrib.returns[, 22])
}
\author{
  Andrii Babii
}
\references{
  Bacon, Carl. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. p. 206-207
}
\seealso{
  \code{\link{Return.annualized}},
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

