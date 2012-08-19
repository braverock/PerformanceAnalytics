\name{TotalRisk}
\alias{TotalRisk}
\title{Total risk of the return distribution}
\usage{
  TotalRisk(Ra, Rb, Rf = 0, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rb}{return vector of the benchmark asset}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{\dots}{any other passthru parameters}
}
\description{
  The square of total risk is the sum of the square of
  systematic risk and the square of specific risk. Specific
  risk is the standard deviation of the error term in the
  regression equation. Both terms are annualized to
  calculate total risk.
}
\details{
  \deqn{Total Risk = \sqrt{Systematic Risk^2 + Specific
  Risk^2}}{Total Risk^2 = Systematic Risk^2 + Specific
  Risk^2}
}
\examples{
data(portfolio_bacon)
print(TotalRisk(portfolio_bacon[,1], portfolio_bacon[,2])) #expected 0.0134

data(managers)
print(TotalRisk(managers['1996',1], managers['1996',8]))
print(TotalRisk(managers['1996',1:5], managers['1996',8]))
}
\author{
  Matthieu Lestel
}
\references{
  Carl Bacon, \emph{Practical portfolio performance
  measurement and attribution}, second edition 2008 p.75
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

