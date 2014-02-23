\name{ProspectRatio}
\alias{ProspectRatio}
\title{Prospect ratio of the return distribution}
\usage{
ProspectRatio(R, MAR, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{MAR}{the minimum acceptable return}

  \item{\dots}{any other passthru parameters}
}
\description{
Prospect ratio is a ratio used to penalise loss since most
people feel loss greater than gain
}
\details{
\deqn{ProspectRatio(R) =
\frac{\frac{1}{n}*\sum^{n}_{i=1}(Max(r_i,0)+2.25*Min(r_i,0)
- MAR)}{\sigma_D}}{ProspectRatio(R) = (1/n * sum(Max(ri,0)
+ 2.25 * Min(ri,0)) - MAR) / DownsideRisk}

where \eqn{n} is the number of observations of the entire
series, MAR is the minimum acceptable return and
\eqn{\sigma_D} is the downside risk
}
\examples{
data(portfolio_bacon)
MAR = 0.05
print(ProspectRatio(portfolio_bacon[,1], MAR)) #expected -0.134

data(managers)
MAR = 0
print(ProspectRatio(managers['1996'], MAR))
print(ProspectRatio(managers['1996',1], MAR))
}
\author{
Matthieu Lestel
}
\references{
Carl Bacon, \emph{Practical portfolio performance
measurement and attribution}, second edition 2008 p.100
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

