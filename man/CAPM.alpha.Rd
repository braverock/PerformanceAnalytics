\name{CAPM.alpha}
\alias{CAPM.alpha}
\title{calculate single factor model (CAPM) alpha}
\usage{
  CAPM.alpha(Ra, Rb, Rf = 0)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rb}{return vector of the benchmark asset}

  \item{Rf}{risk free rate, in same period as your returns}
}
\description{
  This is a wrapper for calculating a single factor model
  (CAPM) alpha.
}
\details{
  "Alpha" purports to be a measure of a manager's skill by
  measuring the portion of the managers returns that are
  not attributable to "Beta", or the portion of performance
  attributable to a benchmark.

  While the classical CAPM has been almost completely
  discredited by the literature, it is an example of a
  simple single factor model, comparing an asset to any
  arbitrary benchmark.
}
\examples{
# First we load the data
    data(managers)
    CAPM.alpha(managers[,1,drop=FALSE],
			managers[,8,drop=FALSE],
			Rf=.035/12)
    CAPM.alpha(managers[,1,drop=FALSE],
			managers[,8,drop=FALSE],
			Rf = managers[,10,drop=FALSE])
    CAPM.alpha(managers[,1:6],
			managers[,8,drop=FALSE],
			Rf=.035/12)
    CAPM.alpha(managers[,1:6],
			managers[,8,drop=FALSE],
			Rf = managers[,10,drop=FALSE])
    CAPM.alpha(managers[,1:6],
			managers[,8:7,drop=FALSE],
			Rf=.035/12)
    CAPM.alpha(managers[,1:6],
			managers[,8:7,drop=FALSE],
			Rf = managers[,10,drop=FALSE])
}
\author{
  Peter Carl
}
\references{
  Sharpe, W.F. Capital Asset Prices: A theory of market
  equilibrium under conditions of risk. \emph{Journal of
  finance}, vol 19, 1964, 425-442. \cr Ruppert, David.
  \emph{Statistics and Finance, an Introduction}. Springer.
  2004. \cr
}
\seealso{
  \code{\link{CAPM.beta}} \code{\link{CAPM.utils}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

