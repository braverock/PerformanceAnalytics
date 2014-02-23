\name{Selectivity}
\alias{Selectivity}
\title{Selectivity of the return distribution}
\usage{
Selectivity(Ra, Rb, Rf = 0, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rb}{return vector of the benchmark asset}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{\dots}{any other passthru parameters}
}
\description{
Selectivity is the same as Jensen's alpha
}
\details{
\deqn{Selectivity = r_p - r_f - \beta_p * (b -
r_f)}{Selectivity = r_p - r_f - beta_p * (b - r_f)}

where \eqn{r_f} is the risk free rate, \eqn{\beta_r} is the
regression beta, \eqn{r_p} is the portfolio return and b is
the benchmark return
}
\examples{
data(portfolio_bacon)
print(Selectivity(portfolio_bacon[,1], portfolio_bacon[,2])) #expected -0.0141

data(managers)
print(Selectivity(managers['1996',1], managers['1996',8]))
print(Selectivity(managers['1996',1:5], managers['1996',8]))
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

