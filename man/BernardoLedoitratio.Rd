\name{BernardoLedoitratio}
\alias{BernardoLedoitratio}
\alias{BernardoLedoitRatio}
\title{Bernardo and Ledoit ratio of the return distribution}
\usage{
  BernardoLedoitratio(R, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{\dots}{any other passthru parameters}
}
\description{
  To calculate Bernado and Ledoit ratio we take the sum of
  the subset of returns that are above 0 and we divide it
  by the opposite of the sum of the subset of returns that
  are below 0
}
\details{
  \deqn{BernadoLedoitratio(R) =
  \frac{\frac{1}{n}\sum^{n}_{t=1}{max(R_{t},0)}}{\frac{1}{n}\sum^{n}_{t=1}
  {max(-R_{t},0)}}}{BernadoLedoitratio(R) = 1/n*sum
  (t=1..n)(max(R(t),0)) / 1/n*sum(t=1..n)(max(-R(t),0))}

  where \eqn{n} is the number of observations of the entire
  series
}
\examples{
data(portfolio_bacon)
print(BernadoLedoitratio(portfolio_bacon)) #expected 1.78

data(managers)
print(BernadoLedoitratio(managers['1996']))
print(BernadoLedoitratio(managers['1996',1])) #expected 4.598
}
\author{
  Matthieu Lestel
}
\references{
  Carl Bacon, \emph{Practical portfolio performance
  measurement and attribution}, second edition 2008 p.95
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

