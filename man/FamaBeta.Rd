\name{FamaBeta}
\alias{FamaBeta}
\title{Fama beta of the return distribution}
\usage{
  FamaBeta(Ra, Rb, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rb}{return vector of the benchmark asset}

  \item{\dots}{any other passthru parameters}
}
\description{
  Fama beta is a beta used to calculate the loss of
  diversification. It is made so that the systematic risk
  is equivalent to the total portfolio risk.
}
\details{
  \deqn{\beta_F = \frac{\sigma_P}{\sigma_M}}{Fama beta =
  portfolio standard deviation / benchmark standard
  deviation}

  where \eqn{\sigma_P} is the portfolio standard deviation
  and \eqn{\sigma_M} is the market risk
}
\examples{
data(portfolio_bacon)
print(FamaBeta(portfolio_bacon[,1], portfolio_bacon[,2])) #expected 1.03

data(managers)
print(FamaBeta(managers['1996',1], managers['1996',8]))
print(FamaBeta(managers['1996',1:5], managers['1996',8]))
}
\author{
  Matthieu Lestel
}
\references{
  Carl Bacon, \emph{Practical portfolio performance
  measurement and attribution}, second edition 2008 p.78
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

