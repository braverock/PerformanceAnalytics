\name{UpsideRisk}
\alias{UpsideRisk}
\title{upside risk, variance and potential of the return distribution}
\usage{
  UpsideRisk(R, MAR = 0, method = c("full", "subset"),
    stat = c("risk", "variance", "potential"), ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{MAR}{Minimum Acceptable Return, in the same
  periodicity as your returns}

  \item{method}{one of "full" or "subset", indicating
  whether to use the length of the full series or the
  length of the subset of the series below the MAR as the
  denominator, defaults to "subset"}

  \item{\dots}{any other passthru parameters}
}
\description{
  Upside Risk is the similar of semideviation taking the
  return above the Minimum Acceptable Return instead of
  using the mean return or zero. To calculate it, we take
  the subset of returns that are more than the target (or
  Minimum Acceptable Returns (MAR)) returns and take the
  differences of those to the target.  We sum the squares
  and divide by the total number of returns and return the
  square root.
}
\details{
  \deqn{ UpsideRisk(R , MAR) = \sqrt{\sum^{n}_{t=1}\frac{
  max[(R_{t} - MAR), 0]^2}{n}}} {UpsideRisk(R, MAR) =
  sqrt(1/n * sum(t=1..n) ((max(R(t)-MAR, 0))^2))}

  \deqn{ UpsideVariance(R, MAR) =
  \sum^{n}_{t=1}\frac{max[(R_{t} - MAR), 0]^2} {n}}
  {UpsideVariance(R, MAR) = 1/n *
  sum(t=1..n)((max(R(t)-MAR, 0))^2)}

  \deqn{UpsidePotential(R, MAR) =
  \sum^{n}_{t=1}\frac{max[(R_{t} - MAR), 0]} {n}}
  {DownsidePotential(R, MAR) = 1/n *
  sum(t=1..n)(max(R(t)-MAR, 0))}

  where \eqn{n} is either the number of observations of the
  entire series or the number of observations in the subset
  of the series falling below the MAR.
}
\examples{
portfolio_return <- c(0.3,2.6,1.1,-1.0,1.5,2.5,1.6,6.7,-1.4,4.0,-0.5,8.1,4.0
,-3.7,-6.1,1.7,-4.9,-2.2,7.0,5.8,-6.5,2.4,-0.5,-0.9)
MAR = 0.5
print(UpsideRisk(portfolio_return, MAR, stat="risk")) #expected 2.937
print(UpsideRisk(portfolio_return, MAR, stat="variance")) #expected 8.628
print(UpsideRisk(portfolio_return, MAR, stat="potential")) #expected 1.771
}
\author{
  Matthieu Lestel
}
\references{
  Carl Bacon, \emph{Practical portfolio performance
  measurement and attribution}, second edition 2008
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

