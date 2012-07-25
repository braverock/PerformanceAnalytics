\name{AppraisalRatio}
\alias{AppraisalRatio}
\title{Appraisal ratio of the return distribution}
\usage{
  AppraisalRatio(Ra, Rb, Rf = 0, period = 12, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rb}{return vector of the benchmark asset}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{period}{number of periods in a year monthly scale =
  12, quarterly = 4)}

  \item{\dots}{any other passthru parameters}
}
\description{
  Appraisal ratio is the Jensen's alpha adjusted for
  systemeatic risk. The numerator is divided by specific
  risk instead of total risk.
}
\details{
  \deqn{Appraisal ratio =
  \frac{alpha}{\sigma_{\epsilon}}}{Appraisal ratio =
  Jensen's alpha / specific risk}

  where \eqn{alpha} is the Jensen's alpha and
  \eqn{\sigma_{epsilon}} is the specific risk.
}
\examples{
data(portfolio_bacon)
print(AppraisalRatio(portfolio_bacon[,1], portfolio_bacon[,2])) #expected -0.0952

data(managers)
print(AppraisalRatio(managers['1996',1], managers['1996',8]))
print(AppraisalRatio(managers['1996',1:5], managers['1996',8]))
}
\author{
  Matthieu Lestel
}
\references{
  Carl Bacon, \emph{Practical portfolio performance
  measurement and attribution}, second edition 2008 p.72
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

